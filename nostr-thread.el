;;; nostr-thread.el --- Nostr thread UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Focused thread view.

;;; Code:

(require 'cl-lib)
(require 'nostr-actions)
(require 'nostr-compose)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-reactions)
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-ui)
(require 'seq)
(require 'transient)

(declare-function nostr-profile-open "nostr-profile" (pubkey))
(declare-function nostr-profile-open-self "nostr-profile" ())
(declare-function nostr-open-identifier "nostr-dispatch" (value))

(defvar-local nostr-thread-root-event nil
  "Root event for the current thread buffer.")

(defvar-local nostr-thread-focus-id nil
  "Event id that should be selected in the current thread buffer.")

(defvar nostr-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-thread-refresh)
    (define-key map (kbd "a") #'nostr-thread-open-author)
    (define-key map (kbd "i") #'nostr-profile-open-self)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-thread-open-at-point)
    (define-key map (kbd "o") #'nostr-thread-open-embedded-reference)
    (define-key map (kbd "r") #'nostr-thread-reply)
    (define-key map (kbd "l") #'nostr-thread-like)
    (define-key map (kbd "v") #'nostr-thread-view-reactions)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "D") #'nostr-ui-show-publish-details)
    (define-key map (kbd "R") #'nostr-thread-repost)
    (define-key map (kbd "Q") #'nostr-thread-quote)
    (define-key map (kbd "w") #'nostr-share-copy)
    (define-key map (kbd "y") #'nostr-share-copy-raw-id)
    (define-key map (kbd "b") #'nostr-share-browse)
    (define-key map (kbd "?") #'nostr-thread-actions)
    map)
  "Keymap for `nostr-thread-mode'.")

(define-derived-mode nostr-thread-mode special-mode "Nostr-Thread"
  "Mode for Nostr thread buffers."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-thread-actions ()
  "Actions for the selected thread note."
  [["Note"
    ("r" "Reply" nostr-thread-reply)
    ("a" "Open author" nostr-thread-open-author)]
   ["Thread"
    ("RET" "Open selected" nostr-thread-open-at-point)
    ("g" "Refresh" nostr-thread-refresh)
    ("i" "My profile" nostr-profile-open-self)
    ("o" "Open embedded" nostr-thread-open-embedded-reference)
    ("D" "Publish details" nostr-ui-show-publish-details)
    ("n" "Next note" nostr-ui-next-section)
    ("p" "Previous note" nostr-ui-prev-section)
    ("TAB" "Toggle note" nostr-ui-toggle-section)]
   ["Social"
    ("l" "React" nostr-thread-like)
    ("v" "View reactions" nostr-thread-view-reactions)
    ("m" "Toggle media" nostr-ui-toggle-note-media)
    ("R" "Repost" nostr-thread-repost)
    ("Q" "Quote" nostr-thread-quote)]
   ["Share"
    ("w" "Copy NIP-19" nostr-share-copy)
    ("y" "Copy raw id" nostr-share-copy-raw-id)
    ("b" "Browse" nostr-share-browse)]])

(defconst nostr-thread-max-depth 32
  "Maximum display nesting depth when walking reply ancestry.
Caps runaway indentation from pathologically deep threads.")

(defun nostr-thread--depth (event events)
  "Return display depth for EVENT in EVENTS.
Walks the `reply-id' ancestry, stopping on cycles (a parent id that
repeats, including self-replies) and clamping at `nostr-thread-max-depth'
so untrusted relay data cannot cause an unbounded loop."
  (let ((depth 0)
        ;; Seed with EVENT's own id so a self-referential reply-id is
        ;; caught immediately rather than looping forever.
        (visited (list (alist-get 'id event)))
        (parent (alist-get 'reply-id event)))
    (while (and parent
                (< depth nostr-thread-max-depth)
                (not (member parent visited)))
      (setq depth (1+ depth))
      (push parent visited)
      (setq parent
            (alist-get 'reply-id
                       (cl-find parent events
                                :key (lambda (candidate)
                                       (alist-get 'id candidate))
                                :test #'equal))))
    depth))

(defun nostr-thread--root-id (event)
  "Return the best known thread root id for EVENT."
  (or (alist-get 'root-id event)
      (alist-get 'root_id event)
      (alist-get 'id event)))

(defun nostr-thread--context-ids (event events)
  "Return event ids worth fetching for thread EVENT and visible EVENTS."
  (delete-dups
   (seq-filter
    #'stringp
    (append
     (list (alist-get 'id event)
           (alist-get 'root-id event)
           (alist-get 'root_id event)
           (alist-get 'reply-id event)
           (alist-get 'reply_id event))
     (mapcar (lambda (candidate) (alist-get 'id candidate)) events)
     (mapcar (lambda (candidate)
               (or (alist-get 'root-id candidate)
                   (alist-get 'root_id candidate)))
             events)
     (mapcar (lambda (candidate)
               (or (alist-get 'reply-id candidate)
                   (alist-get 'reply_id candidate)))
             events)))))

(defun nostr-thread--goto-focus (focus-id)
  "Move point to FOCUS-ID when it is visible in the thread."
  (when-let* ((section (and focus-id (nostr-ui--find-section 'note focus-id))))
    (goto-char (nostr-ui-section-start section))
    (nostr-ui-update-selection)))

(defun nostr-thread-refresh ()
  "Refresh current thread."
  (interactive)
  (let* ((inhibit-read-only t)
         (position-state (nostr-ui-capture-position))
         (point-state (alist-get 'point position-state))
         (root-id (nostr-thread--root-id nostr-thread-root-event))
         (focus-id (or nostr-thread-focus-id root-id))
         (events (nostr-db-select-thread root-id)))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Thread"
     nil
     (if root-id
         (format "Focused conversation for %s." root-id)
       "Focused conversation."))
    ;; Eager avatars for every author in the thread.
    (nostr-relay-fetch-profiles-batch
     (mapcar (lambda (event) (alist-get 'pubkey event)) events))
    (let ((context-ids (nostr-thread--context-ids nostr-thread-root-event events)))
      (nostr-relay-fetch-event-metadata context-ids)
      (nostr-relay-subscribe-visible-reactions context-ids)
      (nostr-relay-fetch-events-by-id context-ids))
    (if events
        (dolist (event events)
          (nostr-ui-insert-note
           event
           (list :depth (nostr-thread--depth event events) :style 'detail)))
      (nostr-ui-insert-empty-state
       "No cached events for this thread."
       "Use g after relay sync or open the event from search."))
    (nostr-ui-insert-footer
     '("g refresh" "r reply" "l react" "R repost" "Q quote" "w copy" "? actions"))
    (nostr-ui-finish-refresh position-state)
    (when (or (not (alist-get 'id point-state))
              (not (nostr-ui-section-at-point)))
      (nostr-thread--goto-focus focus-id))))

(defun nostr-thread-reply ()
  "Reply to selected thread note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-compose-open event)))

(defun nostr-thread-like ()
  "React to selected thread note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-react-menu event)))

(defun nostr-thread-view-reactions ()
  "Show cached reactions for the selected thread note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-reactions-open event)))

(defun nostr-thread-repost ()
  "Repost selected thread note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-repost event)))

(defun nostr-thread-quote ()
  "Quote selected thread note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-quote event)))

(defun nostr-thread-open-author ()
  "Open selected thread note's author profile."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data))
              (pubkey (alist-get 'pubkey event)))
    (require 'nostr-profile)
    (nostr-profile-open pubkey)))

(defun nostr-thread-open-embedded-reference ()
  "Open an embedded public NIP-19 reference from the selected thread note."
  (interactive)
  (let* ((event (or (nostr-ui-selected-data)
                    (user-error "No note selected")))
         (references (seq-filter
                      (lambda (value)
                        (let ((plain (string-remove-prefix
                                      "nostr:"
                                      (string-remove-prefix "@" value))))
                          (or (string-prefix-p "note1" plain)
                              (string-prefix-p "nevent1" plain)
                              (string-prefix-p "naddr1" plain))))
                      (nostr-event-public-identifiers
                       (alist-get 'content event)))))
    (unless references
      (user-error "Selected note has no embedded note reference"))
    (let ((choice (if (= (length references) 1)
                      (car references)
                    (completing-read "Open embedded reference: "
                                     references nil t))))
      (require 'nostr-dispatch)
      (nostr-open-identifier (string-remove-prefix "nostr:" choice)))))

(defalias 'nostr-thread-open-embedded-nevent
  #'nostr-thread-open-embedded-reference)

(defun nostr-thread-open-at-point ()
  "Open an actionable item at point in a thread buffer."
  (interactive)
  (unless (nostr-ui-activate-button-at-point)
    (when-let* ((event (nostr-ui-selected-data)))
      (nostr-thread-open event))))

(defun nostr-thread-open (event)
  "Open the conversation containing EVENT and focus EVENT."
  (let* ((root-id (nostr-thread--root-id event))
         (focus-id (alist-get 'id event))
         (buffer (get-buffer-create (format "*Nostr Thread %s*" root-id))))
    (with-current-buffer buffer
      (nostr-thread-mode)
      (setq-local nostr-thread-root-event event)
      (setq-local nostr-thread-focus-id focus-id)
      (nostr-thread-refresh))
    (pop-to-buffer buffer)))

(provide 'nostr-thread)
;;; nostr-thread.el ends here
