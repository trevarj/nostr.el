;;; nostr-notifications.el --- Nostr notifications buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Operational notification view backed by the local Nostr cache.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'subr-x)
(require 'nostr-db)
(require 'nostr-relay)
(require 'nostr-ui)
(require 'transient)

(declare-function nostr-profile-open "nostr-profile" (pubkey))
(declare-function nostr-search-open "nostr-search" (query))
(declare-function nostr-thread-open "nostr-thread" (event))
(declare-function nostr-timeline-conversations "nostr-timeline" ())
(declare-function nostr-timeline-discover "nostr-timeline" ())
(declare-function nostr-timeline-feed "nostr-timeline" ())
(declare-function nostr-timeline-global "nostr-timeline" ())
(declare-function nostr-timeline-my-posts "nostr-timeline" ())

(defvar nostr-buffer-name)

(defcustom nostr-notifications-limit 100
  "Maximum number of notifications displayed in `nostr-notifications-mode'."
  :type 'integer
  :group 'nostr)

(defcustom nostr-notifications-icon-directory
  (expand-file-name "assets/notifications/"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory containing generated notification type icons."
  :type 'directory
  :group 'nostr)

(defcustom nostr-notifications-icon-size 18
  "Maximum pixel size for notification type icons."
  :type 'integer
  :group 'nostr)

(defvar nostr-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-notifications-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-notifications-open-at-point)
    (define-key map (kbd "a") #'nostr-notifications-open-actor)
    (define-key map (kbd "m") #'nostr-notifications-mark-seen)
    (define-key map (kbd "M") #'nostr-notifications-mark-all-seen)
    (define-key map (kbd "?") #'nostr-notifications-actions)
    ;; Notifications is a primary page, so keep the primary navigation keys
    ;; available after switching away from the timeline mode.
    (define-key map (kbd "f") #'nostr-timeline-feed)
    (define-key map (kbd "h") #'nostr-timeline-feed)
    (define-key map (kbd "C") #'nostr-timeline-conversations)
    (define-key map (kbd "e") #'nostr-timeline-conversations)
    (define-key map (kbd "d") #'nostr-timeline-discover)
    (define-key map (kbd "P") #'nostr-timeline-my-posts)
    (define-key map (kbd "N") #'nostr-notifications-open)
    map)
  "Keymap for `nostr-notifications-mode'.")

(define-derived-mode nostr-notifications-mode special-mode "Nostr-Notifications"
  "Mode for viewing cached Nostr notifications."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-notifications-actions ()
  "Actions for the selected Nostr notification."
  [["Notification"
    ("RET" "Open event" nostr-notifications-open-at-point)
    ("a" "Open actor" nostr-notifications-open-actor)
    ("m" "Mark seen" nostr-notifications-mark-seen)
    ("M" "Mark all seen" nostr-notifications-mark-all-seen)]
   ["Navigation"
    ("g" "Refresh" nostr-notifications-refresh)
    ("f" "Feed" nostr-timeline-feed)
    ("C" "Conversations" nostr-timeline-conversations)
    ("d" "Discover" nostr-timeline-discover)
    ("P" "My posts" nostr-timeline-my-posts)
    ("n" "Next notification" nostr-ui-next-section)
    ("p" "Previous notification" nostr-ui-prev-section)
    ("TAB" "Toggle notification" nostr-ui-toggle-section)]])

(defun nostr-notifications--row-to-alist (row)
  "Convert notification ROW to an alist for display and point selection."
  (pcase row
    (`(,id ,type ,event-id ,actor-pubkey ,target-pubkey ,created-at ,seen
          ,content ,event-pubkey ,name ,display-name ,nip05)
     (let* ((base `((id . ,id)
                    (type . ,type)
                    (event-id . ,event-id)
                    (actor-pubkey . ,actor-pubkey)
                    (target-pubkey . ,target-pubkey)
                    (created-at . ,created-at)
                    (created_at . ,created-at)
                    (seen . ,seen)
                    (content . ,content)
                    (event-pubkey . ,event-pubkey)
                    (actor . ,(nostr-notifications--actor-label
                               actor-pubkey name display-name nip05)))))
       (append base (nostr-notifications--context-data base))))))

(defun nostr-notifications--select (&optional limit)
  "Return cached notifications up to LIMIT."
  (mapcar #'nostr-notifications--row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [notifications:id notifications:type notifications:event_id
                                      notifications:actor_pubkey notifications:target_pubkey
                                      notifications:created_at notifications:seen
                                      events:content events:pubkey
                                      profiles:name profiles:display_name profiles:nip05]
                    :from notifications
                    :left-join events :on (= notifications:event_id events:id)
                    :left-join profiles :on (= notifications:actor_pubkey profiles:pubkey)
                    :order-by [(asc notifications:seen) (desc notifications:created_at)]
                    :limit $s1]
                   (or limit nostr-notifications-limit))))

(defun nostr-notifications-selected ()
  "Return the notification data at point."
  (interactive)
  (nostr-ui-selected-data))

(defun nostr-notifications--selected-or-error ()
  "Return selected notification or signal a user error."
  (or (nostr-notifications-selected)
      (user-error "No notification selected")))

(defun nostr-notifications-open-at-point ()
  "Open the selected notification's thread context or search for it."
  (interactive)
  (let* ((notification (nostr-notifications--selected-or-error))
         (event-id (or (alist-get 'context-event-id notification)
                       (alist-get 'event-id notification))))
    (if event-id
        (if-let* ((event (car (nostr-db-select-thread event-id))))
            (progn
              (require 'nostr-thread)
              (nostr-thread-open event))
          (require 'nostr-search)
          (nostr-search-open event-id))
      (nostr-notifications-open-actor))))

(defun nostr-notifications-open-actor ()
  "Open the selected notification actor profile."
  (interactive)
  (let* ((notification (nostr-notifications--selected-or-error))
         (pubkey (alist-get 'actor-pubkey notification)))
    (unless pubkey
      (user-error "Selected notification has no actor pubkey"))
    (require 'nostr-profile)
    (nostr-profile-open pubkey)))

(defun nostr-notifications-mark-seen ()
  "Mark the selected notification as seen."
  (interactive)
  (when-let* ((notification (nostr-notifications-selected))
              (id (alist-get 'id notification)))
    (nostr-db-mark-notification-seen id)
    (nostr-notifications-refresh)))

(defun nostr-notifications-mark-all-seen ()
  "Mark all notifications as seen."
  (interactive)
  (nostr-db-mark-all-notifications-seen)
  (nostr-notifications-refresh))

(defun nostr-notifications--type-key (type)
  "Return normalized lowercase notification TYPE."
  (if (stringp type)
      (downcase type)
    "notification"))

(defun nostr-notifications--type-label (type)
  "Return a human-readable notification TYPE label."
  (pcase (nostr-notifications--type-key type)
    ("reply" "Reply")
    ("mention" "Mention")
    ("reaction" "Reaction")
    ("repost" "Repost")
    ("zap" "Zap")
    ("follow" "Follow")
    (_ (capitalize (or type "notification")))))

(defun nostr-notifications--type-symbol (type)
  "Return compact fallback symbol for notification TYPE."
  (pcase (nostr-notifications--type-key type)
    ("reply" "↩")
    ("mention" "@")
    ("reaction" "♥")
    ("repost" "↻")
    ("zap" "⚡")
    ("follow" "+")
    (_ "•")))

(defun nostr-notifications--icon-file (type)
  "Return generated icon path for notification TYPE."
  (expand-file-name
   (format "%s.png" (nostr-notifications--type-key type))
   nostr-notifications-icon-directory))

(defun nostr-notifications--icon-string (type)
  "Return inline generated icon or fallback symbol for TYPE."
  (let ((file (nostr-notifications--icon-file type)))
    (if (and (file-exists-p file)
             (display-images-p)
             (image-supported-file-p file))
        (propertize
         " "
         'display (create-image
                   file nil nil
                   :max-width nostr-notifications-icon-size
                   :max-height nostr-notifications-icon-size
                   :ascent 82))
      (propertize (nostr-notifications--type-symbol type)
                  'face 'nostr-ui-meta))))

(defun nostr-notifications--actor-label (pubkey name display-name nip05)
  "Return compact actor label from PUBKEY and profile fields."
  (let ((label (or (and (stringp display-name) display-name)
                   (and (stringp name) name)))
        (identifier (or (and (stringp nip05) nip05)
                        (when-let* ((npub (nostr-ui--cached-npub pubkey)))
                          (nostr-ui--shorten-identifier npub))
                        (nostr-ui--shorten-identifier pubkey))))
    (cond
     ((and label nip05) (format "%s · %s" label nip05))
     (label label)
     (t identifier))))

(defun nostr-notifications--reaction-target (event-id)
  "Return reaction content and target event id for reaction EVENT-ID."
  (pcase (car (emacsql nostr-db--connection
                       [:select [content event_id]
                                :from reactions
                                :where (= id $s1)
                                :limit 1]
                       event-id))
    (`(,content ,target-id)
     `((reaction-content . ,content)
       (context-event-id . ,target-id)))))

(defun nostr-notifications--single-target (table event-id)
  "Return target event id from TABLE for EVENT-ID."
  (when-let* ((target-id
               (caar (emacsql nostr-db--connection
                              `[:select [event_id]
                                       :from ,table
                                       :where (= id $s1)
                                       :limit 1]
                              event-id))))
    `((context-event-id . ,target-id))))

(defun nostr-notifications--event-content (event-id)
  "Return cached content for EVENT-ID."
  (caar (emacsql nostr-db--connection
                 [:select [content] :from events :where (= id $s1) :limit 1]
                 event-id)))

(defun nostr-notifications--context-data (notification)
  "Return extra display and navigation context for NOTIFICATION."
  (let* ((type (alist-get 'type notification))
         (event-id (alist-get 'event-id notification))
         (extra (pcase (nostr-notifications--type-key type)
                  ("reaction" (nostr-notifications--reaction-target event-id))
                  ("repost" (nostr-notifications--single-target 'reposts event-id))
                  ("zap" (nostr-notifications--single-target 'zaps event-id))
                  ("follow" nil)
                  (_ `((context-event-id . ,event-id)))))
         (context-id (alist-get 'context-event-id extra))
         (context-content (and context-id
                               (nostr-notifications--event-content context-id))))
    (append extra `((context-content . ,context-content)))))

(defun nostr-notifications--reaction-display (content)
  "Return user-facing reaction CONTENT."
  (let ((value (string-trim (if (stringp content) content ""))))
    (cond
     ((string-empty-p value) "♥")
     ((equal value "+") "♥")
     ((equal value "-") "👎")
     (t value))))

(defun nostr-notifications--action-text (notification)
  "Return compact action text for NOTIFICATION."
  (pcase (nostr-notifications--type-key (alist-get 'type notification))
    ("reply" "replied")
    ("mention" "mentioned you")
    ("reaction" (format "reacted %s"
                         (nostr-notifications--reaction-display
                          (alist-get 'reaction-content notification))))
    ("repost" "reposted")
    ("zap" "zapped")
    ("follow" "followed you")
    (_ (downcase (nostr-notifications--type-label
                  (alist-get 'type notification))))))

(defun nostr-notifications--summary (notification)
  "Return one-line summary for NOTIFICATION."
  (let* ((raw (or (alist-get 'context-content notification)
                  (alist-get 'content notification)))
         (content (string-trim (if (stringp raw) raw ""))))
    (if (string-empty-p content)
        ""
      (truncate-string-to-width content 76 nil nil t))))

(defun nostr-notifications--unread-count (notifications)
  "Return unread count in NOTIFICATIONS."
  (cl-count-if (lambda (notification)
                 (zerop (or (alist-get 'seen notification) 0)))
               notifications))

(defun nostr-notifications--insert-notification (notification)
  "Insert NOTIFICATION as a selectable UI section."
  (let* ((id (alist-get 'id notification))
         (type (alist-get 'type notification))
         (actor (alist-get 'actor notification))
         (created-at (alist-get 'created-at notification))
         (seen (not (zerop (or (alist-get 'seen notification) 0))))
         (time (nostr-ui-format-time created-at 'feed)))
    (nostr-ui-with-section 'notification id notification
        (lambda (section)
          (insert-text-button
           "▾"
           'follow-link t
           'nostr-ui-section section
           'action (lambda (_button) (nostr-ui-toggle-section)))
          (insert " ")
          (insert (nostr-notifications--icon-string type))
          (insert " ")
          (unless seen
            (insert (propertize "● " 'face 'nostr-ui-section-heading)))
          (insert (propertize (or actor "(unknown)") 'face 'nostr-ui-author))
          (insert (format " %s  " (nostr-notifications--action-text notification)))
          (insert (propertize time 'face 'nostr-ui-meta))
          (insert "\n"))
      (insert (propertize (nostr-notifications--summary notification)
                          'face 'nostr-ui-content))
      (insert "\n")
      (nostr-ui-insert-badge-line
       (delq nil
             (list
              (when (alist-get 'context-event-id notification) "RET thread")
              (unless (alist-get 'context-event-id notification) "RET profile")
              (when-let* ((event-id (or (alist-get 'context-event-id notification)
                                        (alist-get 'event-id notification))))
                (format "event %s" (nostr-ui--shorten-identifier event-id)))))
       "  ")
      (insert "\n"))))

(defun nostr-notifications-refresh ()
  "Refresh the current notifications buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (position-state (nostr-ui-capture-position)))
    (nostr-ui-clear)
    (if nostr-db--connection
        (let ((notifications (nostr-notifications--select)))
          (nostr-ui-insert-status-header
           "Notifications"
           nil
           (format "%d unread  %d total"
                   (nostr-notifications--unread-count notifications)
                   (length notifications)))
          (nostr-ui-insert-primary-nav nostr-ui-primary-nav-items 'notifications)
          (let ((ids (mapcar (lambda (notification)
                                (alist-get 'event-id notification))
                              notifications)))
            (nostr-relay-fetch-event-metadata ids)
            (nostr-relay-subscribe-visible-reactions ids))
          (if notifications
              (dolist (notification notifications)
                (nostr-notifications--insert-notification notification))
            (nostr-ui-insert-empty-state
             "No cached notifications."
             "Mentions, replies, reactions, reposts, and follows will appear here.")))
      (insert (propertize "Database is not open.\n" 'face 'nostr-ui-meta)))
    (nostr-ui-insert-footer
     '("g refresh" "f feed" "C conv" "d discover" "P posts" "RET open" "? actions"))
    (nostr-ui-finish-refresh position-state)))

;;;###autoload
(defun nostr-notifications-open ()
  "Open the Nostr notifications view.
Use the main `nostr-buffer-name' buffer when it exists so Notifications behaves
like the other primary navigation tabs.  Fall back to a standalone buffer only
when the main Nostr buffer has not been created yet."
  (interactive)
  (let ((main-buffer (and (boundp 'nostr-buffer-name)
                          (get-buffer nostr-buffer-name))))
    (cond
     ((or (eq major-mode 'nostr-timeline-mode)
          (eq major-mode 'nostr-notifications-mode))
      (nostr-notifications-mode)
      (nostr-notifications-refresh))
     (main-buffer
      (switch-to-buffer main-buffer)
      (nostr-notifications-mode)
      (nostr-notifications-refresh))
     (t
      (nostr-notifications-open-standalone)))))

(defun nostr-notifications-open-standalone ()
  "Open the standalone Nostr notifications buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Nostr Notifications*")))
    (with-current-buffer buffer
      (nostr-notifications-mode)
      (nostr-notifications-refresh))
    (switch-to-buffer buffer)))

(provide 'nostr-notifications)
;;; nostr-notifications.el ends here
