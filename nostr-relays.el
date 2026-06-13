;;; nostr-relays.el --- Nostr relay status buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Operational relay status view backed by the local Nostr cache.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'websocket)
(require 'nostr-db)
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-ui)
(require 'transient)

(defvar nostr-current-pubkey)
(defvar nostr-relay--connections)

(defun nostr-relays--candidate-urls ()
  "Return relay URLs that should be displayed as candidates."
  (if (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
      (nostr-relay-urls-for-pubkey nostr-current-pubkey)
    nostr-relay-urls))

(defun nostr-relays--relay-preference (url)
  "Return cached NIP-65 preference for URL."
  (when (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (cl-find url (nostr-db-select-relay-list nostr-current-pubkey)
             :key (lambda (relay) (alist-get 'url relay))
             :test #'equal)))

(defun nostr-relays--relay-source (url)
  "Return a display source for relay URL."
  (cond
   ((member url nostr-relay-urls) "configured")
   ((nostr-relays--relay-preference url)
    "nip65")
   (t "cached")))

(defun nostr-relays--display-policy (url preference)
  "Return display read/write policy for URL using PREFERENCE."
  (cond
   (preference (cons (alist-get 'read preference)
                     (alist-get 'write preference)))
   ((member url nostr-relay-urls) '(t . t))
   (t '(nil . nil))))

(defvar nostr-relays-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-relays-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "a") #'nostr-relays-add)
    (define-key map (kbd "c") #'nostr-relays-connect)
    (define-key map (kbd "d") #'nostr-relays-disconnect)
    (define-key map (kbd "k") #'nostr-relays-remove)
    (define-key map (kbd "w") #'nostr-relays-copy-url)
    (define-key map (kbd "?") #'nostr-relays-actions)
    map)
  "Keymap for `nostr-relays-mode'.")

(define-derived-mode nostr-relays-mode special-mode "Nostr-Relays"
  "Mode for viewing and controlling Nostr relay connections."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-relays-actions ()
  "Actions for the selected Nostr relay."
  [["Relays"
    ("g" "Refresh" nostr-relays-refresh)
    ("a" "Add" nostr-relays-add)
    ("c" "Connect" nostr-relays-connect)
    ("d" "Disconnect" nostr-relays-disconnect)
    ("k" "Remove" nostr-relays-remove)]
   ["Navigation"
    ("n" "Next relay" nostr-ui-next-section)
    ("p" "Previous relay" nostr-ui-prev-section)
    ("TAB" "Toggle relay" nostr-ui-toggle-section)]
   ["Share"
    ("w" "Copy URL" nostr-relays-copy-url)]])

(defun nostr-relays--row-to-alist (row)
  "Convert relay status ROW to an alist for display and point selection."
  (pcase row
    (`(,url ,state ,message ,updated-at)
     (let* ((preference (nostr-relays--relay-preference url))
            (policy (nostr-relays--display-policy url preference)))
       `((url . ,url)
         (state . ,state)
         (message . ,message)
         (source . ,(nostr-relays--relay-source url))
         (marker . ,(alist-get 'marker preference))
         (read . ,(car policy))
         (write . ,(cdr policy))
         (updated-at . ,updated-at)
         (updated_at . ,updated-at))))))

(defun nostr-relays--configured-statuses ()
  "Return relay rows for candidate relays missing from `relay_status'."
  (let ((stored (mapcar #'car (nostr-db-select-relays))))
    (delq nil
          (mapcar
           (lambda (url)
             (unless (member url stored)
               (let* ((preference (nostr-relays--relay-preference url))
                      (policy (nostr-relays--display-policy url preference)))
                 `((url . ,url)
                   (state . "candidate")
                   (message . nil)
                   (source . ,(nostr-relays--relay-source url))
                   (marker . ,(alist-get 'marker preference))
                   (read . ,(car policy))
                   (write . ,(cdr policy))
                   (updated-at . nil)
                   (updated_at . nil)))))
           (nostr-relays--candidate-urls)))))

(defun nostr-relays--select ()
  "Return relay statuses, including configured relays without cached state."
  (sort (append (mapcar #'nostr-relays--row-to-alist (nostr-db-select-relays))
                (nostr-relays--configured-statuses))
        (lambda (a b)
          (string< (or (alist-get 'url a) "") (or (alist-get 'url b) "")))))

(defun nostr-relays-selected ()
  "Return relay data at point."
  (interactive)
  (nostr-ui-selected-data))

(defun nostr-relays--live-state (url)
  "Return live websocket state string for URL."
  (let ((ws (and (boundp 'nostr-relay--connections)
                 (gethash url nostr-relay--connections))))
    (cond
     ((and ws (websocket-openp ws)) "open")
     (ws "closed")
     (t "not-connected"))))

(defun nostr-relays--insert-relay (relay)
  "Insert RELAY as a selectable UI section."
  (let* ((url (alist-get 'url relay))
         (state (or (alist-get 'state relay) "unknown"))
         (message (alist-get 'message relay))
         (source (or (alist-get 'source relay) "cached"))
         (read (alist-get 'read relay))
         (write (alist-get 'write relay))
         (updated-at (alist-get 'updated-at relay))
         (live (nostr-relays--live-state url))
         (title (format "%s  source:%s  cache:%s  live:%s"
                        (or url "(unknown relay)") source state live)))
    (nostr-ui-with-section 'relay url relay title
      (insert (propertize
               (format "Last update: %s\n"
                       (if updated-at
                           (nostr-ui-format-time updated-at)
                         "never"))
               'face 'nostr-ui-meta))
      (insert (propertize
               (format "Policy: read=%s write=%s\n"
                       (if read "yes" "no")
                       (if write "yes" "no"))
               'face 'nostr-ui-meta))
      (insert (propertize
               (format "Message: %s\n\n" (or message "-"))
               'face 'nostr-ui-content)))))

(defun nostr-relays--summary (relays)
  "Return compact RELAYS status summary."
  (let ((open 0)
        (closed 0)
        (candidate 0))
    (dolist (relay relays)
      (pcase (nostr-relays--live-state (alist-get 'url relay))
        ("open" (setq open (1+ open)))
        ("not-connected" (setq candidate (1+ candidate)))
        (_ (setq closed (1+ closed)))))
    (format "%d open  %d closed  %d not connected"
            open closed candidate)))

(defun nostr-relays-refresh ()
  "Refresh the current relays buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (position-state (nostr-ui-capture-position)))
    (nostr-ui-clear)
    (if nostr-db--connection
        (let ((relays (nostr-relays--select)))
          (nostr-ui-insert-status-header
           "Relays"
           "Nostr"
           (nostr-relays--summary relays))
          (if relays
              (dolist (relay relays)
                (nostr-relays--insert-relay relay))
            (nostr-ui-insert-empty-state
             "No configured relays."
             "Use a to add a relay URL.")))
      (insert (propertize "Database is not open.\n" 'face 'nostr-ui-meta)))
    (nostr-ui-insert-footer
     '("g refresh" "a add" "c connect" "d disconnect" "k remove" "w copy" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-relays--selected-url ()
  "Return selected relay URL or signal a user error."
  (or (alist-get 'url (nostr-ui-selected-data))
      (user-error "No relay selected")))

(defun nostr-relays-connect ()
  "Connect the selected relay."
  (interactive)
  (let ((url (nostr-relays--selected-url)))
    (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
      (user-error "No current Nostr pubkey is available"))
    (nostr-relay-open url nostr-current-pubkey)
    (nostr-db-store-relay-status url "connecting")
    (nostr-relays-refresh)))

(defun nostr-relays-add (url)
  "Add URL to the session relay list."
  (interactive (list (read-string "Relay URL: " "wss://")))
  (unless (string-prefix-p "wss://" url)
    (user-error "Relay URL must start with wss://"))
  (cl-pushnew url nostr-relay-urls :test #'equal)
  (setq nostr-relay-urls (sort nostr-relay-urls #'string<))
  (nostr-relays-refresh))

(defun nostr-relays-remove ()
  "Remove the selected configured relay from the session relay list."
  (interactive)
  (let ((url (nostr-relays--selected-url)))
    (unless (member url nostr-relay-urls)
      (user-error "Only configured relays can be removed locally"))
    (setq nostr-relay-urls (delete url nostr-relay-urls))
    (nostr-relays-disconnect)
    (nostr-relays-refresh)))

(defun nostr-relays-copy-url ()
  "Copy the selected relay URL."
  (interactive)
  (nostr-share-copy-relay-url))

(defun nostr-relays-disconnect ()
  "Disconnect the selected relay."
  (interactive)
  (let* ((url (nostr-relays--selected-url))
         (ws (gethash url nostr-relay--connections)))
    (when (and ws (websocket-openp ws))
      (websocket-close ws))
    (remhash url nostr-relay--connections)
    (nostr-db-store-relay-status url "closed" "Disconnected locally")
    (nostr-relays-refresh)))

;;;###autoload
(defun nostr-relays-open ()
  "Open the Nostr relay status buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Nostr Relays*")))
    (with-current-buffer buffer
      (nostr-relays-mode)
      (nostr-relays-refresh))
    (pop-to-buffer buffer)))

(provide 'nostr-relays)
;;; nostr-relays.el ends here
