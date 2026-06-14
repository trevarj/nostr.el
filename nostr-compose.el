;;; nostr-compose.el --- Nostr compose buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Compose and send public notes.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-nip)
(require 'nostr-relay)
(require 'nostr-ui)
(require 'transient)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-request-method)

(defvar-local nostr-compose-reply-to nil
  "Event being replied to.")

(defvar-local nostr-compose-extra-tags nil
  "Extra tags to add to the note being composed.")

(defvar-local nostr-compose-dirty nil
  "Whether compose content has changed since the buffer opened.")

(defvar-local nostr-compose--sent nil
  "Whether the current compose buffer has been successfully sent.")

(defvar-local nostr-compose-content-start nil
  "Marker for the start of editable compose content.")

(defvar-local nostr-compose-draft-file nil
  "Autosave file for this compose buffer.")

(defvar-local nostr-compose--draft-cycle-entries nil
  "Draft history entries currently being cycled in this compose buffer.")

(defvar-local nostr-compose--draft-cycle-index nil
  "Current index into `nostr-compose--draft-cycle-entries'.")

(defvar-local nostr-compose--restoring-draft nil
  "Whether this compose buffer is currently restoring draft content.")

(defvar-local nostr-compose--draft-save-timer nil
  "Pending autosave timer for the current compose buffer.")

(defvar-local nostr-compose--uploading nil
  "Whether this compose buffer is uploading attachments.")

(defvar nostr-actions-after-send-hook nil
  "Hook run with the signed event after a public action is sent.")

(defcustom nostr-compose-draft-directory
  (expand-file-name "nostr-drafts/" user-emacs-directory)
  "Directory where unsent compose drafts are autosaved."
  :type 'directory
  :group 'nostr)

(defcustom nostr-compose-draft-history-file nil
  "File storing reusable compose draft history.
When nil, use history.el inside `nostr-compose-draft-directory'."
  :type '(choice (const :tag "Use draft directory history.el" nil) file)
  :group 'nostr)

(defcustom nostr-compose-draft-history-length 50
  "Maximum number of compose drafts kept in reusable history."
  :type 'integer
  :group 'nostr)

(defcustom nostr-compose-mention-completion-limit 200
  "Maximum cached profiles offered for compose mention completion."
  :type 'integer
  :group 'nostr)

(defcustom nostr-compose-display-buffer-action nil
  "Optional `display-buffer' action used for compose buffers.
When nil, compose opens in a side window, right on wide frames and bottom on
smaller frames."
  :type '(choice (const :tag "Automatic side window" nil) sexp)
  :group 'nostr)

(defcustom nostr-blossom-upload-server "https://blossom.primal.net"
  "Blossom server URL used for compose media attachment uploads."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'nostr)

(defcustom nostr-blossom-upload-auth-expiration-seconds 300
  "Seconds before generated Blossom upload authorization expires."
  :type 'integer
  :group 'nostr)

(defvar nostr-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nostr-compose-send)
    (define-key map (kbd "C-c C-k") #'nostr-compose-cancel)
    (define-key map (kbd "C-c C-a") #'nostr-compose-attach-file)
    (define-key map (kbd "M-p") #'nostr-compose-previous-draft)
    (define-key map (kbd "M-n") #'nostr-compose-next-draft)
    (define-key map (kbd "@") #'nostr-compose-insert-mention-trigger)
    (define-key map (kbd "?") #'nostr-compose-actions)
    map)
  "Keymap for `nostr-compose-mode'.")

(define-derived-mode nostr-compose-mode text-mode "Nostr-Compose"
  "Mode for writing a public Nostr note."
  (setq-local header-line-format '(:eval (nostr-compose--header-line)))
  (add-hook 'completion-at-point-functions #'nostr-compose-complete-mention nil t)
  (add-hook 'after-change-functions #'nostr-compose--after-change nil t)
  (add-hook 'kill-buffer-query-functions #'nostr-compose--confirm-kill nil t))

(transient-define-prefix nostr-compose-actions ()
  "Actions for the current Nostr compose buffer."
  [["Compose"
    ("C-c C-c" "Send" nostr-compose-send)
    ("C-c C-a" "Attach file" nostr-compose-attach-file)
    ("C-c C-k" "Cancel" nostr-compose-cancel)
    ("M-p" "Previous draft" nostr-compose-previous-draft)
    ("M-n" "Next draft" nostr-compose-next-draft)
    ("R" "Restore latest" nostr-compose-restore-draft)]])

(defun nostr-compose--after-change (&rest _ignored)
  "Mark current compose buffer as dirty after content changes."
  (when (and (not nostr-compose--restoring-draft)
             (or (not (markerp nostr-compose-content-start))
                 (>= (point-max) nostr-compose-content-start)))
    (setq nostr-compose--draft-cycle-entries nil)
    (setq nostr-compose--draft-cycle-index nil)
    (setq nostr-compose-dirty t)
    (nostr-compose--schedule-save-draft)))

(defun nostr-compose--content ()
  "Return editable compose buffer content."
  (string-trim
   (buffer-substring-no-properties
    (or nostr-compose-content-start (point-min))
    (point-max))))

(defun nostr-compose--draft-content ()
  "Return compose content suitable for draft persistence."
  (buffer-substring-no-properties
   (or nostr-compose-content-start (point-min))
   (point-max)))

(defun nostr-compose--target-label (event)
  "Return compact label for target EVENT."
  (or (alist-get 'author event)
      (alist-get 'pubkey event)
      (alist-get 'id event)
      "unknown"))

(defun nostr-compose--quote-event-id ()
  "Return quoted event id from `nostr-compose-extra-tags'."
  (cadr (car (nostr-event-tags-by-name nostr-compose-extra-tags "q"))))

(defun nostr-compose--quote-event ()
  "Return cached quoted event for compose context."
  (when-let* ((quote-id (nostr-compose--quote-event-id)))
    (car (nostr-db-select-thread quote-id))))

(defun nostr-compose--context-events ()
  "Return context events for the current compose buffer."
  (delq nil
        (list nostr-compose-reply-to
              (or (nostr-compose--quote-event)
                  (when-let* ((quote-id (nostr-compose--quote-event-id)))
                    `((id . ,quote-id)
                      (pubkey . ,quote-id)
                      (created-at . nil)
                      (content . "(quote target is not cached)")))))))

(defun nostr-compose--insert-context ()
  "Insert readonly reply and quote context."
  (let ((events (nostr-compose--context-events)))
    (when events
      (let ((start (point))
            (nostr-ui-show-avatars nil))
        (insert (propertize "Context\n" 'face 'nostr-ui-status-title))
        (dolist (event events)
          (nostr-ui-insert-note event '(:style detail)))
        (insert "\n")
        (add-text-properties start (point)
                             '(read-only t front-sticky t rear-nonsticky t))))))

(defun nostr-compose--ensure-content-start ()
  "Ensure `nostr-compose-content-start' marks editable content."
  (unless (markerp nostr-compose-content-start)
    (setq nostr-compose-content-start (copy-marker (point-min)))))

(defun nostr-compose--header-line ()
  "Return compose header line text."
  (let* ((content (nostr-compose--content))
         (count (length content))
         (attachments (length (nostr-compose--attachment-paths content)))
         (parts (delq nil
                      (list "Nostr Compose"
                            (when nostr-compose-reply-to
                              (format "reply: %s"
                                      (nostr-compose--target-label nostr-compose-reply-to)))
                            (when (nostr-compose--quote-event-id)
                              (format "quote: %s" (nostr-compose--quote-event-id)))
                            (when (> attachments 0)
                              (format "%d attachment%s" attachments
                                      (if (= attachments 1) "" "s")))
                            (when nostr-compose--uploading "uploading")
                            (format "%d chars" count)))))
    (string-join parts "  |  ")))

(defun nostr-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsent content."
  (cond
   (nostr-compose--sent t)
   ((not nostr-compose-dirty) t)
   ((string-empty-p (nostr-compose--content)) t)
   ((yes-or-no-p "Discard unsent Nostr draft? ")
    (nostr-compose--record-draft)
    t)
   (t nil)))

(defconst nostr-compose--attachment-regexp
  "\\[attachment:\\([^]\n]+\\)\\]"
  "Regexp matching inline compose attachment tokens.")

(defun nostr-compose--attachment-paths (content)
  "Return attachment paths found in CONTENT."
  (let (paths)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward nostr-compose--attachment-regexp nil t)
        (push (match-string-no-properties 1) paths)))
    (delete-dups (nreverse paths))))

(defun nostr-compose--sha256-file (file)
  "Return SHA-256 hex digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (buffer-string))))

(defun nostr-compose--upload-url ()
  "Return configured Blossom upload endpoint."
  (unless (and (stringp nostr-blossom-upload-server)
               (not (string-empty-p nostr-blossom-upload-server)))
    (user-error "Set `nostr-blossom-upload-server' before sending attachments"))
  (concat (string-remove-suffix "/" nostr-blossom-upload-server) "/upload"))

(defun nostr-compose--parse-blossom-url (json-text)
  "Return uploaded media URL from Blossom JSON-TEXT."
  (let* ((parsed (json-parse-string json-text :object-type 'alist :array-type 'list
                                    :false-object nil))
         (url (or (alist-get 'url parsed)
                  (alist-get 'download_url parsed)
                  (alist-get 'downloadUrl parsed)))
         (nip94 (alist-get 'nip94_event parsed)))
    (or url
        (when-let* ((tags (alist-get 'tags nip94)))
          (cadr (seq-find (lambda (tag)
                            (and (listp tag) (equal (car tag) "url")))
                          tags)))
        (error "Blossom upload response did not include a URL"))))

(defun nostr-compose--replace-attachments (content uploads)
  "Replace attachment tokens in CONTENT using UPLOADS alist."
  (replace-regexp-in-string
   nostr-compose--attachment-regexp
   (lambda (token)
     (if (string-match nostr-compose--attachment-regexp token)
         (or (cdr (assoc (match-string 1 token) uploads)) token)
       token))
   content t t))

(defun nostr-compose--http-unibyte (string)
  "Return STRING encoded as unibyte HTTP header/body text."
  (if (multibyte-string-p string)
      (encode-coding-string string 'us-ascii)
    string))

(defun nostr-compose--sanitize-upload-error (message)
  "Return upload error MESSAGE without embedded binary request data."
  (if (string-match "\\`Multibyte text in HTTP request:" message)
      "Could not build binary upload request"
    (truncate-string-to-width message 240 nil nil "...")))

(defun nostr-compose--read-file-bytes (file)
  "Return unibyte contents of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (encode-coding-string (buffer-string) 'no-conversion)))

(defun nostr-compose--upload-file (file success error)
  "Upload FILE to the configured Blossom server."
  (unless (file-readable-p file)
    (user-error "Attachment is not readable: %s" file))
  (let* ((upload-url (nostr-compose--upload-url))
         (server (string-remove-suffix "/" nostr-blossom-upload-server))
         (sha256 (nostr-compose--sha256-file file))
         (expiration (+ (truncate (float-time))
                        nostr-blossom-upload-auth-expiration-seconds)))
    (nostr-backend-blossom-auth
     server sha256 expiration
     (lambda (auth)
       (let ((url-request-method "PUT")
             (url-request-data (nostr-compose--read-file-bytes file))
             (url-request-extra-headers
              `((,(nostr-compose--http-unibyte "Authorization")
                 . ,(nostr-compose--http-unibyte
                     (alist-get 'authorization auth)))
                (,(nostr-compose--http-unibyte "Content-Type")
                 . ,(nostr-compose--http-unibyte
                     "application/octet-stream")))))
         (condition-case err
             (url-retrieve
              upload-url
              (lambda (status)
                (unwind-protect
                    (if-let* ((err (plist-get status :error)))
                        (funcall error file
                                 (format "Could not upload %s: %s"
                                         file
                                         (nostr-compose--sanitize-upload-error
                                          (format "%S" err))))
                      (if (not (and url-http-response-status
                                    (>= url-http-response-status 200)
                                    (< url-http-response-status 300)))
                          (funcall error file
                                   (format "Could not upload %s: HTTP %s"
                                           file
                                           (or url-http-response-status "unknown")))
                        (condition-case err
                            (funcall success file
                                     (nostr-compose--parse-blossom-url
                                      (buffer-substring-no-properties
                                       (1+ url-http-end-of-headers)
                                       (point-max))))
                          (error (funcall error file
                                          (nostr-compose--sanitize-upload-error
                                           (error-message-string err)))))))
                  (kill-buffer (current-buffer))))
              nil t)
           (error
            (funcall error file
                     (format "Could not upload %s: %s"
                             file
                             (nostr-compose--sanitize-upload-error
                              (error-message-string err))))))))
     (lambda (response stderr _status)
       (funcall error file
                (format "Could not authorize %s: %s%s"
                        file
                        (or (alist-get 'message (alist-get 'error response))
                            "backend error")
                        (if (string-empty-p stderr) "" (concat ": " stderr))))))))

(defun nostr-compose--upload-attachments (paths success error &optional uploads)
  "Upload PATHS, then call SUCCESS with UPLOADS alist."
  (if (null paths)
      (funcall success (nreverse uploads))
    (nostr-compose--upload-file
     (car paths)
     (lambda (file url)
       (nostr-compose--upload-attachments
        (cdr paths) success error (cons (cons file url) uploads)))
     error)))

(defun nostr-compose-attach-file (file)
  "Insert an inline attachment token for FILE."
  (interactive "fAttach file: ")
  (nostr-compose--ensure-content-start)
  (goto-char (max (point) nostr-compose-content-start))
  (unless (bolp) (insert "\n"))
  (insert (format "[attachment:%s]\n" (expand-file-name file))))

(defun nostr-compose--draft-data ()
  "Return current draft data."
  `((reply-to . ,nostr-compose-reply-to)
    (extra-tags . ,nostr-compose-extra-tags)
    (content . ,(nostr-compose--draft-content))
    (updated-at . ,(truncate (float-time)))))

(defun nostr-compose--draft-history-path ()
  "Return the compose draft history file path."
  (or nostr-compose-draft-history-file
      (expand-file-name "history.el" nostr-compose-draft-directory)))

(defun nostr-compose--read-draft-data (file)
  "Read draft data from FILE, returning nil if it cannot be read."
  (when (and file (file-readable-p file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error nil))))

(defun nostr-compose--same-draft-p (a b)
  "Return non-nil when draft A and B represent the same reusable content."
  (and (equal (alist-get 'content a) (alist-get 'content b))
       (equal (alist-get 'reply-to a) (alist-get 'reply-to b))
       (equal (alist-get 'extra-tags a) (alist-get 'extra-tags b))))

(defun nostr-compose--usable-draft-p (draft)
  "Return non-nil when DRAFT has reusable content."
  (and (listp draft)
       (not (string-empty-p
             (string-trim (or (alist-get 'content draft) ""))))))

(defun nostr-compose--read-draft-history ()
  "Return saved compose draft history entries."
  (let ((history (nostr-compose--read-draft-data
                  (nostr-compose--draft-history-path))))
    (if (listp history) history nil)))

(defun nostr-compose--write-draft-history (history)
  "Persist compose draft HISTORY."
  (make-directory (file-name-directory (nostr-compose--draft-history-path)) t)
  (with-temp-file (nostr-compose--draft-history-path)
    (let ((print-length nil)
          (print-level nil))
      (prin1 history (current-buffer)))))

(defun nostr-compose--record-draft (&optional draft)
  "Record DRAFT or the current buffer content in reusable history."
  (let ((entry (or draft (nostr-compose--draft-data))))
    (when (nostr-compose--usable-draft-p entry)
      (let* ((history (cl-remove-if
                       (lambda (candidate)
                         (nostr-compose--same-draft-p entry candidate))
                       (nostr-compose--read-draft-history)))
             (limit (max 1 nostr-compose-draft-history-length)))
        (nostr-compose--write-draft-history
         (seq-take (cons entry history) limit))))))

(defun nostr-compose--save-draft-buffer (buffer)
  "Autosave compose draft for BUFFER when it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq nostr-compose--draft-save-timer nil)
      (nostr-compose--save-draft))))

(defun nostr-compose--schedule-save-draft ()
  "Schedule autosave after the current edit finishes."
  (when (timerp nostr-compose--draft-save-timer)
    (cancel-timer nostr-compose--draft-save-timer))
  (setq nostr-compose--draft-save-timer
        (run-at-time 0 nil #'nostr-compose--save-draft-buffer
                     (current-buffer))))

(defun nostr-compose--save-draft ()
  "Autosave the current compose buffer."
  (let ((content (and (markerp nostr-compose-content-start)
                      (nostr-compose--draft-content))))
    (when (and (derived-mode-p 'nostr-compose-mode)
               (not nostr-compose--sent)
               (not nostr-compose--uploading)
               (not (string-empty-p (string-trim (or content "")))))
    (make-directory nostr-compose-draft-directory t)
    (unless nostr-compose-draft-file
      (setq nostr-compose-draft-file
            (expand-file-name
             (format "draft-%s.el" (format-time-string "%Y%m%d%H%M%S%N"))
             nostr-compose-draft-directory)))
    (with-temp-file nostr-compose-draft-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 (nostr-compose--draft-data) (current-buffer)))))))

(defun nostr-compose--delete-draft ()
  "Delete the current draft autosave file."
  (when (and nostr-compose-draft-file
             (file-exists-p nostr-compose-draft-file))
    (delete-file nostr-compose-draft-file)))

(defun nostr-compose--draft-files ()
  "Return autosaved draft files, newest first."
  (when (file-directory-p nostr-compose-draft-directory)
    (let ((history-path (expand-file-name (nostr-compose--draft-history-path))))
      (sort (cl-remove-if
             (lambda (file)
               (equal (expand-file-name file) history-path))
             (directory-files nostr-compose-draft-directory t "\\.el\\'"))
            (lambda (a b)
              (time-less-p (file-attribute-modification-time (file-attributes b))
                           (file-attribute-modification-time (file-attributes a))))))))

(defun nostr-compose--latest-draft ()
  "Return the newest autosaved or historical compose draft."
  (or (seq-some (lambda (file)
                  (let ((draft (nostr-compose--read-draft-data file)))
                    (and (nostr-compose--usable-draft-p draft) draft)))
                (nostr-compose--draft-files))
      (seq-find #'nostr-compose--usable-draft-p
                (nostr-compose--read-draft-history))))

(defun nostr-compose--draft-candidates (&optional skip-current)
  "Return draft candidates for cycling.
When SKIP-CURRENT is non-nil, omit entries matching current buffer content."
  (let* ((autosaves (seq-filter #'nostr-compose--usable-draft-p
                                (delq nil (mapcar #'nostr-compose--read-draft-data
                                                  (nostr-compose--draft-files)))))
         (entries (seq-filter #'nostr-compose--usable-draft-p
                              (append autosaves (nostr-compose--read-draft-history))))
         result)
    (dolist (entry entries (nreverse result))
      (unless (or (and skip-current
                       (nostr-compose--same-draft-p
                        (nostr-compose--draft-data)
                        entry))
                  (seq-some (lambda (seen)
                              (nostr-compose--same-draft-p entry seen))
                            result))
        (push entry result)))))

(defun nostr-compose--apply-draft (draft)
  "Replace the current compose buffer with DRAFT."
  (let ((inhibit-read-only t)
        (nostr-compose--restoring-draft t))
    (erase-buffer)
    (setq nostr-compose-reply-to (alist-get 'reply-to draft))
    (setq nostr-compose-extra-tags (alist-get 'extra-tags draft))
    (nostr-compose--insert-context)
    (setq nostr-compose-content-start (copy-marker (point)))
    (insert (or (alist-get 'content draft) ""))
    (setq nostr-compose-dirty t)
    (force-mode-line-update)))

(defun nostr-compose-restore-draft ()
  "Restore the most recent unsent compose draft."
  (interactive)
  (let ((draft (or (nostr-compose--latest-draft)
                   (user-error "No Nostr compose drafts found"))))
    (if (derived-mode-p 'nostr-compose-mode)
        (progn
          (nostr-compose--apply-draft draft)
          (nostr-compose--save-draft)
          (message "Nostr draft restored"))
      (nostr-compose-open (alist-get 'reply-to draft)
                          (alist-get 'extra-tags draft)
                          (alist-get 'content draft)))))

(defun nostr-compose--cycle-draft (step)
  "Cycle compose draft history by STEP."
  (unless (derived-mode-p 'nostr-compose-mode)
    (user-error "Not in a Nostr compose buffer"))
  (unless nostr-compose--draft-cycle-entries
    (setq nostr-compose--draft-cycle-entries
          (nostr-compose--draft-candidates t))
    (setq nostr-compose--draft-cycle-index nil))
  (unless nostr-compose--draft-cycle-entries
    (user-error "No previous Nostr drafts found"))
  (setq nostr-compose--draft-cycle-index
        (mod (+ (or nostr-compose--draft-cycle-index
                    (if (> step 0) -1 0))
                step)
             (length nostr-compose--draft-cycle-entries)))
  (nostr-compose--apply-draft
   (nth nostr-compose--draft-cycle-index nostr-compose--draft-cycle-entries))
  (nostr-compose--save-draft)
  (message "Nostr draft %d/%d"
           (1+ nostr-compose--draft-cycle-index)
           (length nostr-compose--draft-cycle-entries)))

(defun nostr-compose-previous-draft ()
  "Restore the previous saved compose draft, like commit message history."
  (interactive)
  (nostr-compose--cycle-draft 1))

(defun nostr-compose-next-draft ()
  "Restore the next saved compose draft, like commit message history."
  (interactive)
  (nostr-compose--cycle-draft -1))

(defun nostr-compose--completion-candidates ()
  "Return cached profile completion candidates as LABEL/PUBKEY pairs."
  (and nostr-db--connection
       (let ((seen (make-hash-table :test #'equal))
             candidates)
         (dolist (row (nostr-db-select-profile-completions
                       nostr-compose-mention-completion-limit))
           (pcase-let ((`(,pubkey ,name ,display-name ,nip05) row))
             (dolist (label (delq nil (list display-name name nip05 pubkey)))
               (when (and (stringp label)
                          (not (string-empty-p label))
                          (not (gethash label seen)))
                 (puthash label t seen)
                 (push (cons label pubkey) candidates)))))
         (nreverse candidates))))

(defun nostr-compose--mention-bounds ()
  "Return completion bounds for an @mention at point."
  (let ((end (point))
        start)
    (save-excursion
      (skip-chars-backward "[:alnum:]_.-")
      (setq start (point)))
    (when (and (> start (point-min))
               (eq (char-before start) ?@)
               (or (= (1- start) (point-min))
                   (memq (char-syntax (char-before (1- start)))
                         '(?\s ?. ?\( ?\)))))
      (cons start end))))

(defun nostr-compose-insert-mention-trigger ()
  "Insert @ and start cached profile completion."
  (interactive)
  (insert "@")
  (completion-at-point))

(defun nostr-compose-complete-mention ()
  "Complete cached profile mentions near point."
  (let ((bounds (nostr-compose--mention-bounds)))
    (when bounds
      (when-let* ((pairs (nostr-compose--completion-candidates)))
        (let* ((table (mapcar #'car pairs))
               (start-marker (copy-marker (car bounds)))
               (end-marker (copy-marker (cdr bounds) t)))
          (list (car bounds) (cdr bounds)
                (completion-table-dynamic (lambda (_string) table))
                :annotation-function
                (lambda (candidate)
                  (let ((pubkey (cdr (assoc candidate pairs))))
                    (when pubkey
                      (concat " " (truncate-string-to-width pubkey 12 nil nil "...")))))
                :exit-function
                (lambda (candidate status)
                  (when (eq status 'finished)
                    (let ((pubkey (cdr (assoc candidate pairs))))
                      (when pubkey
                        (delete-region (1- start-marker) end-marker)
                        (insert "nostr:"
                                (alist-get 'value
                                           (nostr-nip19-encode-sync "npub" pubkey)))))))))))))

(defun nostr-compose--display-action ()
  "Return automatic display action for compose buffers."
  (or nostr-compose-display-buffer-action
      (let ((wide (>= (frame-width) 120)))
        `((display-buffer-in-side-window)
          (side . ,(if wide 'right 'bottom))
          (slot . 0)
          ,@(if wide
                '((window-width . 0.38))
              '((window-height . 0.34)))))))

(defun nostr-compose--display-buffer (buffer)
  "Display and select compose BUFFER."
  (select-window (display-buffer buffer (nostr-compose--display-action))))

(defun nostr-compose-open (&optional reply-to extra-tags initial-content draft-file)
  "Open a compose buffer, optionally replying to REPLY-TO.
EXTRA-TAGS are added to the sent event.  INITIAL-CONTENT and DRAFT-FILE are
used when restoring an autosaved draft."
  (interactive)
  (let ((buffer (generate-new-buffer "*Nostr Compose*")))
    (with-current-buffer buffer
      (nostr-compose-mode)
      (setq nostr-compose-reply-to reply-to)
      (setq nostr-compose-extra-tags extra-tags)
      (setq nostr-compose-draft-file draft-file)
      (nostr-compose--insert-context)
      (setq nostr-compose-content-start (copy-marker (point)))
      (when initial-content
        (insert initial-content))
      (setq nostr-compose-dirty nil))
    (nostr-compose--display-buffer buffer)))

(defun nostr-compose-cancel (&optional force)
  "Cancel the current compose buffer.
With FORCE, close without prompting."
  (interactive "P")
  (when (or force (nostr-compose--confirm-kill))
    (let ((nostr-compose--sent t))
      (unless (string-empty-p (nostr-compose--content))
        (nostr-compose--record-draft))
      (nostr-compose--delete-draft)
      (kill-buffer (current-buffer)))
    (message "Nostr compose cancelled")))

(defun nostr-compose--sign-and-send (content buffer)
  "Sign CONTENT and send it for compose BUFFER."
  (let* ((reply-to nostr-compose-reply-to)
         (tags (append (nostr-event-build-reply-tags reply-to (car nostr-relay-urls))
                       nostr-compose-extra-tags)))
    (nostr-backend-sign-event
     nostr-kind-text-note
     tags
     content
     (lambda (response)
       (let* ((event (alist-get 'event response))
              (sent-count (nostr-relay-send-client-message
                           (alist-get 'client_message response))))
         (when event
           (nostr-db-store-event (nostr-event-normalize event nil))
           (run-hook-with-args 'nostr-actions-after-send-hook event))
         (message "Nostr note queued to %d relay%s"
                  sent-count
                  (if (= sent-count 1) "" "s")))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (nostr-compose--record-draft)
           (setq nostr-compose--sent t)
           (nostr-compose--delete-draft))
         (kill-buffer buffer)))
     (lambda (response stderr _status)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq nostr-compose--uploading nil)
           (force-mode-line-update)))
       (message "Nostr signing failed: %s%s"
                (or (alist-get 'message (alist-get 'error response)) "backend error")
                (if (string-empty-p stderr) "" (concat ": " stderr)))))))

(defun nostr-compose-send ()
  "Upload attachments, then sign and send the current compose buffer."
  (interactive)
  (let* ((content (nostr-compose--content))
         (paths (nostr-compose--attachment-paths content))
         (buffer (current-buffer)))
    (cond
     ((string-empty-p content)
      (user-error "Cannot send an empty Nostr note"))
     (paths
      (setq nostr-compose--uploading t)
      (force-mode-line-update)
      (nostr-compose--upload-attachments
       paths
       (lambda (uploads)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq nostr-compose--uploading nil)
             (nostr-compose--sign-and-send
              (nostr-compose--replace-attachments content uploads)
              buffer))))
       (lambda (_file message)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq nostr-compose--uploading nil)
             (force-mode-line-update)))
         (message "%s" message))))
     (t
      (nostr-compose--sign-and-send content buffer)))))

(provide 'nostr-compose)
;;; nostr-compose.el ends here
