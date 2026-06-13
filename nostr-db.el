;;; nostr-db.el --- Nostr SQLite storage -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr
;; Package-Requires: ((emacs "30.1") (emacsql "3.1.1"))

;;; Commentary:

;; Fresh cache schema for public-social Nostr data.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'json)
(require 'nostr-event)
(require 'seq)
(require 'subr-x)

(defvar nostr-db--connection nil
  "Current SQLite connection.")

(defun nostr-db-open (path)
  "Open Nostr database at PATH and initialize the schema."
  (setq nostr-db--connection (emacsql-sqlite-open path))
  (nostr-db-init)
  nostr-db--connection)

(defun nostr-db-close ()
  "Close the current Nostr database connection."
  (when nostr-db--connection
    (emacsql-close nostr-db--connection)
    (setq nostr-db--connection nil)))

(defun nostr-db--table-columns (table)
  "Return column names for TABLE in the current SQLite database."
  (mapcar (lambda (row)
            (symbol-name (nth 1 row)))
          (emacsql nostr-db--connection
                   `[:pragma (= table_info ,(intern table))])))

(defun nostr-db--ensure-column (table column &optional definition)
  "Ensure TABLE has COLUMN, adding it with DEFINITION when missing."
  (unless (member column (nostr-db--table-columns table))
    (emacsql nostr-db--connection
             (format "alter table %s add column %s %s"
                     table
                     column
                     (or definition "")))))

(defun nostr-db--migrate-existing-schema ()
  "Repair older cache schemas in-place.
  `create-table if not exists' intentionally preserves existing user cache data,
so new columns must be added explicitly."
  (nostr-db--ensure-column "events" "root_id")
  (nostr-db--ensure-column "events" "reply_id")
  (nostr-db--ensure-column "events" "quote_id")
  (nostr-db--ensure-column "profiles" "lud16")
  (nostr-db--ensure-column "follows" "relay")
  (nostr-db--ensure-column "follows" "petname")
  (nostr-db--ensure-column "notifications" "seen" "integer default 0"))

(defun nostr-db-init ()
  "Initialize a fresh v1 database schema."
  (emacsql nostr-db--connection
           [:create-table :if-not-exists meta
                          ([(key :primary-key) value])])
  (emacsql nostr-db--connection
           [:insert-or-ignore :into meta :values ["schema-version" "1"]])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists events
                          ([(id :primary-key)
                            pubkey
                            (created_at integer)
                            (kind integer)
                            tags
                            content
                            sig
                            relay
                            root_id
                            reply_id
                            quote_id])])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_events_created_at :on events ([created_at])])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_events_pubkey :on events ([pubkey])])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_events_kind :on events ([kind])])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_events_root_id :on events ([root_id])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists profiles
                          ([(pubkey :primary-key)
                            name
                            display_name
                            about
                            picture
                            nip05
                            lud16
                            (updated_at integer)])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists follows
                          ([pubkey contact relay petname]
                           (:unique [pubkey contact]))])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists reactions
                          ([(id :primary-key)
                            event_id
                            pubkey
                            content
                            (created_at integer)])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists reposts
                          ([(id :primary-key)
                            event_id
                            pubkey
                            (created_at integer)])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists zaps
                          ([(id :primary-key)
                            event_id
                            pubkey
                            (amount_msats integer)
                            (created_at integer)])])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_zaps_event_id :on zaps ([event_id])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists relay_status
                          ([(url :primary-key)
                            state
                            message
                            (updated_at integer)])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists publish_receipts
                          ([event_id
                            url
                            state
                            message
                            (updated_at integer)]
                           (:unique [event_id url]))])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_publish_receipts_event
                          :on publish_receipts ([event_id])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists relay_preferences
                          ([pubkey
                            url
                            marker
                            (read integer)
                            (write integer)]
                           (:unique [pubkey url marker]))])
  (emacsql nostr-db--connection
           [:create-index :if-not-exists idx_relay_preferences_pubkey
                          :on relay_preferences ([pubkey])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists notifications
                          ([(id :primary-key)
                            type
                            event_id
                            actor_pubkey
                            target_pubkey
                            (created_at integer)
                            (seen integer :default 0)])])
  (emacsql nostr-db--connection
           [:create-table :if-not-exists media
                          ([(url :primary-key)
                            path
                            content_type
                            (bytes integer)
                            state
                            message
                            (updated_at integer)])])
  (nostr-db--migrate-existing-schema))

(defun nostr-db-reset (path)
  "Delete database at PATH and recreate it."
  (nostr-db-close)
  (when (file-exists-p path)
    (delete-file path))
  (nostr-db-open path))

(defun nostr-db--profile-name-from-json (content key &rest aliases)
  "Return KEY or one of ALIASES from metadata JSON CONTENT."
  (let ((parsed (ignore-errors
                  (json-parse-string content
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))))
    (seq-some (lambda (field)
                (let ((value (alist-get field parsed)))
                  (when (and (stringp value)
                             (not (string-empty-p value)))
                    value)))
              (cons key aliases))))

(defun nostr-db-store-profile-event (event)
  "Store kind 0 metadata EVENT."
  (let ((content (alist-get 'content event))
        (pubkey (alist-get 'pubkey event))
        (created-at (or (alist-get 'created_at event) 0)))
  (emacsql nostr-db--connection
             [:insert-or-replace :into profiles
                                 [pubkey name display_name about picture nip05 lud16 updated_at]
                                 :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8]]
             pubkey
             (nostr-db--profile-name-from-json content 'name 'username)
             (nostr-db--profile-name-from-json content 'display_name 'displayName)
             (nostr-db--profile-name-from-json content 'about)
             (nostr-db--profile-name-from-json content 'picture)
             (nostr-db--profile-name-from-json content 'nip05)
             (nostr-db--profile-name-from-json content 'lud16)
             created-at)))

(defun nostr-db-store-follows-event (event)
  "Store kind 3 follows EVENT."
  (let ((pubkey (alist-get 'pubkey event)))
    (emacsql-with-transaction nostr-db--connection
      (emacsql nostr-db--connection [:delete-from follows :where (= pubkey $s1)] pubkey)
      (dolist (tag (nostr-event-tags-by-name (alist-get 'tags event) "p"))
        (emacsql nostr-db--connection
                 [:insert-or-ignore :into follows
                                    [pubkey contact relay petname]
                                    :values [$s1 $s2 $s3 $s4]]
                 pubkey (nth 1 tag) (nth 2 tag) (nth 3 tag))))))

(defun nostr-db-store-text-event (event)
  "Store kind 1 text EVENT."
  (emacsql nostr-db--connection
           [:insert-or-replace :into events
                               [id pubkey created_at kind tags content sig relay
                                   root_id reply_id quote_id]
                               :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9 $s10 $s11]]
           (alist-get 'id event)
           (alist-get 'pubkey event)
           (alist-get 'created_at event)
           (alist-get 'kind event)
           (alist-get 'tags event)
           (alist-get 'content event)
           (alist-get 'sig event)
           (alist-get 'relay event)
           (alist-get 'root-id event)
           (alist-get 'reply-id event)
           (alist-get 'quote-id event)))

(defun nostr-db-store-reaction-event (event)
  "Store kind 7 reaction EVENT."
  (when-let* ((event-id (nostr-event-reaction-event-id event)))
    (emacsql nostr-db--connection
             [:insert-or-ignore :into reactions :values [$s1 $s2 $s3 $s4 $s5]]
             (alist-get 'id event)
             event-id
             (alist-get 'pubkey event)
             (alist-get 'content event)
             (alist-get 'created_at event))))

(defun nostr-db-store-repost-event (event)
  "Store kind 6 repost EVENT."
  (when-let* ((event-id (nostr-event-repost-event-id event)))
    (emacsql nostr-db--connection
             [:insert-or-ignore :into reposts :values [$s1 $s2 $s3 $s4]]
             (alist-get 'id event)
             event-id
             (alist-get 'pubkey event)
             (alist-get 'created_at event))))

(defun nostr-db-store-zap-event (event)
  "Store kind 9735 zap receipt EVENT."
  (when-let* ((event-id (nostr-event-zap-target-event-id event)))
    (emacsql nostr-db--connection
             [:insert-or-ignore :into zaps :values [$s1 $s2 $s3 $s4 $s5]]
             (alist-get 'id event)
             event-id
             (alist-get 'pubkey event)
             (nostr-event-zap-amount-msats event)
             (alist-get 'created_at event))))

(defun nostr-db--relay-list-policy (marker)
  "Return cons of read/write booleans for NIP-65 MARKER."
  (pcase marker
    ("read" '(1 . 0))
    ("write" '(0 . 1))
    (_ '(1 . 1))))

(defun nostr-db-store-relay-list-event (event)
  "Store kind 10002 relay list metadata EVENT."
  (let ((pubkey (alist-get 'pubkey event)))
    (emacsql-with-transaction nostr-db--connection
      (emacsql nostr-db--connection
               [:delete-from relay_preferences :where (= pubkey $s1)]
               pubkey)
      (dolist (tag (nostr-event-tags-by-name (alist-get 'tags event) "r"))
        (when-let* ((url (nth 1 tag))
                    ((not (string-empty-p url))))
          (let* ((marker (nth 2 tag))
                 (policy (nostr-db--relay-list-policy marker)))
            (emacsql nostr-db--connection
                     [:insert-or-replace :into relay_preferences
                                         :values [$s1 $s2 $s3 $s4 $s5]]
                     pubkey url marker (car policy) (cdr policy))))))))

(defun nostr-db-store-event (event)
  "Store normalized EVENT."
  (pcase (alist-get 'kind event)
    (0 (nostr-db-store-profile-event event))
    (1 (nostr-db-store-text-event event))
	    (3 (nostr-db-store-follows-event event))
	    (6 (nostr-db-store-repost-event event))
	    (7 (nostr-db-store-reaction-event event))
	    (9735 (nostr-db-store-zap-event event))
	    (10002 (nostr-db-store-relay-list-event event))))

(defun nostr-db-store-relay-status (url state &optional message)
  "Store relay URL STATE and optional MESSAGE."
  (emacsql nostr-db--connection
           [:insert-or-replace :into relay_status :values [$s1 $s2 $s3 $s4]]
           url state message (truncate (float-time))))

(defun nostr-db-store-publish-receipt (event-id url state &optional message)
  "Store publish receipt for EVENT-ID sent to relay URL."
  (emacsql nostr-db--connection
           [:insert-or-replace :into publish_receipts :values [$s1 $s2 $s3 $s4 $s5]]
           event-id url state message (truncate (float-time))))

(defun nostr-db-select-publish-receipts (event-id)
  "Return publish receipts for EVENT-ID ordered by relay URL."
  (mapcar (lambda (row)
            (pcase-let ((`(,url ,state ,message ,updated-at) row))
              `((url . ,url)
                (state . ,state)
                (message . ,message)
                (updated-at . ,updated-at))))
          (emacsql nostr-db--connection
                   [:select [url state message updated_at]
                            :from publish_receipts
                            :where (= event_id $s1)
                            :order-by [(asc url)]]
                   event-id)))

(defun nostr-db-event-pubkey (event-id)
  "Return pubkey for EVENT-ID."
  (caar (emacsql nostr-db--connection
                 [:select [pubkey] :from events :where (= id $s1)]
                 event-id)))

(defun nostr-db-store-notification (id type event-id actor-pubkey target-pubkey created-at)
  "Store notification ID of TYPE for EVENT-ID."
  (emacsql nostr-db--connection
           [:insert-or-ignore :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 0]]
           id type event-id actor-pubkey target-pubkey created-at))

(defun nostr-db-mark-notification-seen (id)
  "Mark notification ID as seen."
  (emacsql nostr-db--connection
           [:update notifications :set (= seen 1) :where (= id $s1)]
           id))

(defun nostr-db-mark-all-notifications-seen ()
  "Mark all notifications as seen."
  (emacsql nostr-db--connection
           [:update notifications :set (= seen 1)]))

(defun nostr-db-select-follows (pubkey)
  "Return PUBKEY's followed pubkeys."
  (mapcar #'car
          (emacsql nostr-db--connection
                   [:select [contact] :from follows :where (= pubkey $s1)]
                   pubkey)))

(defun nostr-db-follows-p (pubkey contact)
  "Return non-nil when PUBKEY follows CONTACT."
  (not (null (emacsql nostr-db--connection
                      [:select [contact]
                               :from follows
                               :where (and (= pubkey $s1)
                                           (= contact $s2))
                               :limit 1]
                      pubkey contact))))

(defun nostr-db-following-count (pubkey)
  "Return cached number of accounts PUBKEY follows."
  (or (caar (emacsql nostr-db--connection
                     [:select [(funcall count *)]
                              :from follows
                              :where (= pubkey $s1)]
                     pubkey))
      0))

(defun nostr-db-follower-count (pubkey)
  "Return cached number of accounts that follow PUBKEY."
  (or (caar (emacsql nostr-db--connection
                     [:select [(funcall count *)]
                              :from follows
                              :where (= contact $s1)]
                     pubkey))
      0))

(defun nostr-db-latest-event-time (&optional pubkeys)
  "Return newest cached event timestamp, optionally limited to PUBKEYS."
  (if pubkeys
      (caar (emacsql nostr-db--connection
                     [:select [(funcall max created_at)]
                              :from events
                              :where (in pubkey $v1)]
                     (vconcat pubkeys)))
    (caar (emacsql nostr-db--connection
                   [:select [(funcall max created_at)] :from events]))))

(defun nostr-db-oldest-latest-event-time (pubkeys)
  "Return the oldest per-author latest event timestamp for PUBKEYS."
  (when pubkeys
    (let ((latest-times
           (mapcar #'cadr
                   (emacsql nostr-db--connection
                            [:select [pubkey (funcall max created_at)]
                             :from events
                             :where (in pubkey $v1)
                             :group-by [pubkey]]
                            (vconcat pubkeys)))))
      (when latest-times
        (seq-min latest-times)))))

(defun nostr-db--zero-counts ()
  "Return a fresh zeroed interaction-counts alist."
  (list (cons 'reactions 0)
        (cons 'reposts 0)
        (cons 'replies 0)
        (cons 'zaps 0)
        (cons 'zap-msats 0)))

(defun nostr-db-event-counts-batch (event-ids)
  "Return an alist mapping each of EVENT-IDS to its interaction counts.
Each value has the same shape as `nostr-db-event-counts'.  Ids with no
interactions get zeroed counts.  This runs a fixed number of grouped queries
for the whole batch instead of five queries per id, so rendering a feed of N
notes costs O(1) round-trips rather than O(N).

Counts are loaded separately from feed queries so feeds do not eagerly scan the
reaction/repost/reply tables for rows that may never be rendered."
  (let ((ids (delete-dups (seq-filter #'stringp event-ids)))
        (result (make-hash-table :test #'equal)))
    (dolist (id ids)
      (puthash id (nostr-db--zero-counts) result))
    (when ids
      (let ((vids (vconcat ids)))
        (pcase-dolist (`(,id ,n)
                       (emacsql nostr-db--connection
                                [:select [event_id (funcall count *)]
                                         :from reactions
                                         :where (in event_id $v1)
                                         :group-by event_id]
                                vids))
          (when-let* ((row (gethash id result)))
            (setf (alist-get 'reactions row) (or n 0))))
        (pcase-dolist (`(,id ,n)
                       (emacsql nostr-db--connection
                                [:select [event_id (funcall count *)]
                                         :from reposts
                                         :where (in event_id $v1)
                                         :group-by event_id]
                                vids))
          (when-let* ((row (gethash id result)))
            (setf (alist-get 'reposts row) (or n 0))))
        (pcase-dolist (`(,id ,n ,sum)
                       (emacsql nostr-db--connection
                                [:select [event_id (funcall count *) (funcall sum amount_msats)]
                                         :from zaps
                                         :where (in event_id $v1)
                                         :group-by event_id]
                                vids))
          (when-let* ((row (gethash id result)))
            (setf (alist-get 'zaps row) (or n 0))
            (setf (alist-get 'zap-msats row) (or sum 0))))
        ;; Replies: any event referencing a target via reply_id OR root_id,
        ;; deduped per target so an event whose reply_id and root_id are the same
        ;; target (a common NIP-10 direct reply) is counted once -- matching the
        ;; single-id query's `(or (= reply_id ..) (= root_id ..))'.
        (let ((reply-sets (make-hash-table :test #'equal)))
          (pcase-dolist (`(,eid ,reply-id ,root-id)
                         (emacsql nostr-db--connection
                                  [:select [id reply_id root_id]
                                           :from events
                                           :where (or (in reply_id $v1)
                                                      (in root_id $v1))]
                                  vids))
            (dolist (target (delete-dups (delq nil (list reply-id root-id))))
              (when (gethash target result)
                (let ((set (or (gethash target reply-sets)
                               (puthash target (make-hash-table :test #'equal)
                                        reply-sets))))
                  (puthash eid t set)))))
          (maphash (lambda (target set)
                     (when-let* ((row (gethash target result)))
                       (setf (alist-get 'replies row) (hash-table-count set))))
                   reply-sets))))
    (let (out)
      (dolist (id ids)
        (push (cons id (gethash id result)) out))
      (nreverse out))))

(defun nostr-db-event-counts (event-id)
  "Return cached interaction counts for EVENT-ID.
Shape: an alist of `reactions', `reposts', `replies', `zaps' and `zap-msats'.
Delegates to `nostr-db-event-counts-batch' so there is a single query path."
  (or (cdr (car (nostr-db-event-counts-batch (list event-id))))
      (nostr-db--zero-counts)))

(defun nostr-db--event-row-to-alist (row)
  "Convert joined event ROW to an alist."
  (pcase row
    (`(,id ,pubkey ,created-at ,kind ,tags ,content ,sig ,relay ,root-id ,reply-id
          ,quote-id ,name ,display-name ,picture)
     `((id . ,id)
       (pubkey . ,pubkey)
       (created-at . ,created-at)
       (created_at . ,created-at)
       (kind . ,kind)
       (tags . ,tags)
       (content . ,content)
       (sig . ,sig)
       (relay . ,relay)
       (root-id . ,root-id)
       (reply-id . ,reply-id)
       (quote-id . ,quote-id)
       (author . ,(or display-name name pubkey))
       (picture . ,picture)))
    (`(,id ,pubkey ,created-at ,kind ,tags ,content ,sig ,relay ,root-id ,reply-id
          ,quote-id ,name ,display-name ,picture ,reactions ,reposts ,replies)
     `((id . ,id)
       (pubkey . ,pubkey)
       (created-at . ,created-at)
       (created_at . ,created-at)
       (kind . ,kind)
       (tags . ,tags)
       (content . ,content)
       (sig . ,sig)
       (relay . ,relay)
       (root-id . ,root-id)
       (reply-id . ,reply-id)
       (quote-id . ,quote-id)
       (author . ,(or display-name name pubkey))
       (picture . ,picture)
       (reactions . ,reactions)
       (reposts . ,reposts)
       (replies . ,replies)))
    (`(,id ,pubkey ,created-at ,kind ,tags ,content ,sig ,relay ,root-id ,reply-id
          ,quote-id ,name ,display-name ,picture ,reposted-by ,reposted-at
          ,reposter-name ,reposter-display-name ,reposter-nip05)
     `((id . ,id)
       (pubkey . ,pubkey)
       (created-at . ,created-at)
       (created_at . ,created-at)
       (kind . ,kind)
       (tags . ,tags)
       (content . ,content)
       (sig . ,sig)
       (relay . ,relay)
       (root-id . ,root-id)
       (reply-id . ,reply-id)
       (quote-id . ,quote-id)
       (author . ,(or display-name name pubkey))
       (picture . ,picture)
       (reposted-by . ,reposted-by)
       (reposted-at . ,reposted-at)
       (reposted-by-name . ,(or reposter-display-name reposter-name))
       (reposted-by-nip05 . ,reposter-nip05)))))

(defun nostr-db--feed-sort-time (event)
  "Return the timestamp used to position EVENT in a feed."
  (or (alist-get 'reposted-at event)
      (alist-get 'created-at event)
      (alist-get 'created_at event)
      0))

(defun nostr-db--take (items limit)
  "Return at most LIMIT ITEMS."
  (seq-take items (or limit 100)))

(defun nostr-db--select-reposted-feed (pubkey limit root-only)
  "Return notes reposted by contacts of PUBKEY."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   `[:select
                     [events:id events:pubkey events:created_at events:kind events:tags
                                events:content events:sig events:relay events:root_id events:reply_id
                                events:quote_id profiles:name profiles:display_name profiles:picture
                                reposts:pubkey reposts:created_at
                                reposter:name reposter:display_name reposter:nip05]
                     :from reposts
                     :inner-join follows :on (and (= reposts:pubkey follows:contact)
                                                  (= follows:pubkey $s1))
                     :inner-join events :on (= reposts:event_id events:id)
                     :left-join profiles :on (= events:pubkey profiles:pubkey)
                     :left-join profiles :as reposter :on (= reposts:pubkey reposter:pubkey)
                     :where (and (= events:kind 1)
                                 ,(if root-only '(is events:root_id nil) 'true))
                     :order-by [(desc reposts:created_at)]
                     :limit $s2]
                   pubkey (or limit 100))))

(defun nostr-db-select-missing-repost-targets (pubkey &optional limit)
  "Return missing event ids reposted by contacts of PUBKEY."
  (mapcar #'car
          (emacsql nostr-db--connection
                   [:select [reposts:event_id]
                    :from reposts
                    :inner-join follows :on (and (= reposts:pubkey follows:contact)
                                                 (= follows:pubkey $s1))
                    :left-join events :on (= reposts:event_id events:id)
                    :where (is events:id nil)
                    :order-by [(desc reposts:created_at)]
                    :limit $s2]
                   pubkey (or limit 100))))

(defun nostr-db-select-feed (pubkey &optional limit root-only include-self)
  "Return feed events from contacts of PUBKEY.
LIMIT defaults to 100.  If ROOT-ONLY is non-nil, exclude replies.  When
INCLUDE-SELF may be `exclude-self' to omit PUBKEY's own notes; other values
preserve the historical behavior of including PUBKEY."
  (let* ((limit (or limit 100))
         (include-own (not (eq include-self 'exclude-self)))
         (notes (mapcar #'nostr-db--event-row-to-alist
                        (emacsql nostr-db--connection
                                 `[:select
                                   [events:id events:pubkey events:created_at events:kind events:tags
                                              events:content events:sig events:relay events:root_id events:reply_id
                                              events:quote_id profiles:name profiles:display_name profiles:picture]
                                   :from events
                                   :left-join follows :on (and (= events:pubkey follows:contact)
                                                               (= follows:pubkey $s1))
                                   :left-join profiles :on (= events:pubkey profiles:pubkey)
                                   :where (and (= events:kind 1)
                                               (or ,(if include-own '(= events:pubkey $s1) 'false)
                                                   (is-not follows:contact nil))
                                               ,(if root-only '(is events:root_id nil) 'true))
                                   :order-by [(desc events:created_at)]
                                   :limit $s2]
                                 pubkey limit)))
         (reposts (nostr-db--select-reposted-feed pubkey limit root-only)))
    (nostr-db--take
     (sort (append notes reposts)
           (lambda (left right)
             (> (nostr-db--feed-sort-time left)
                (nostr-db--feed-sort-time right))))
     limit)))

(defun nostr-db-select-account-feed (pubkey &optional limit)
  "Return root feed notes from accounts followed by PUBKEY."
  (nostr-db-select-feed pubkey limit t 'exclude-self))

(defun nostr-db-select-home-feed (pubkey &optional limit)
  "Return root home feed notes for PUBKEY.
This compatibility helper includes PUBKEY's own notes."
  (nostr-db-select-feed pubkey limit t t))

(defun nostr-db-select-conversations-feed (pubkey &optional limit)
  "Return replies from accounts followed by PUBKEY."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [events:id events:pubkey events:created_at events:kind events:tags
                               events:content events:sig events:relay events:root_id events:reply_id
                               events:quote_id profiles:name profiles:display_name profiles:picture]
                    :from events
                    :left-join follows :on (and (= events:pubkey follows:contact)
                                                (= follows:pubkey $s1))
                    :left-join profiles :on (= events:pubkey profiles:pubkey)
                    :where (and (= events:kind 1)
                                (is-not follows:contact nil)
                                (is-not events:root_id nil))
                    :order-by [(desc events:created_at)]
                    :limit $s2]
                   pubkey (or limit 100))))

(defun nostr-db-select-replies-feed (pubkey &optional limit)
  "Return replies from accounts followed by PUBKEY.
Compatibility alias for `nostr-db-select-conversations-feed'."
  (nostr-db-select-conversations-feed pubkey limit))

(defun nostr-db-select-global-feed (&optional limit)
  "Return recent notes from the local cache."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [events:id events:pubkey events:created_at events:kind events:tags
                               events:content events:sig events:relay events:root_id events:reply_id
                               events:quote_id profiles:name profiles:display_name profiles:picture]
                    :from events
                    :left-join profiles :on (= events:pubkey profiles:pubkey)
                    :where (= events:kind 1)
                    :order-by [(desc events:created_at)]
                    :limit $s1]
                   (or limit 100))))

(defun nostr-db-select-author-feed (pubkey &optional limit)
  "Return cached notes authored by PUBKEY."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [events:id events:pubkey events:created_at events:kind events:tags
                               events:content events:sig events:relay events:root_id events:reply_id
                               events:quote_id profiles:name profiles:display_name profiles:picture]
                    :from events
                    :left-join profiles :on (= events:pubkey profiles:pubkey)
                    :where (and (= events:kind 1)
                                (= events:pubkey $s1))
                    :order-by [(desc events:created_at)]
                    :limit $s2]
                   pubkey (or limit 100))))

(defun nostr-db-select-media-feed (pubkey &optional limit)
  "Return home feed notes for PUBKEY that contain likely image URLs."
  (seq-filter
   (lambda (event)
     (nostr-event-media-urls (alist-get 'content event)))
   (nostr-db-select-feed pubkey (or limit 100) nil)))

(defun nostr-db-select-thread (root-id)
  "Return root event and replies for ROOT-ID."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [events:id events:pubkey events:created_at events:kind events:tags
                               events:content events:sig events:relay events:root_id events:reply_id
                               events:quote_id profiles:name profiles:display_name profiles:picture]
                    :from events
                    :left-join profiles :on (= events:pubkey profiles:pubkey)
                    :where (or (= events:id $s1)
                               (= events:root_id $s1)
                               (= events:reply_id $s1))
                    :order-by [(asc events:created_at)]]
                   root-id)))

(defun nostr-db-select-profile (pubkey)
  "Return profile for PUBKEY."
  (car (emacsql nostr-db--connection
                [:select [pubkey name display_name about picture nip05 lud16 updated_at]
                         :from profiles
                         :where (= pubkey $s1)]
                pubkey)))

(defun nostr-db-select-profiles-batch (pubkeys)
  "Return an alist mapping each present PUBKEY to its profile row.
Each value matches the row shape of `nostr-db-select-profile'.  Pubkeys with no
stored profile are absent from the result.  Runs one query for the whole batch."
  (let ((ids (delete-dups (seq-filter #'stringp pubkeys))))
    (when ids
      (mapcar (lambda (row) (cons (car row) row))
              (emacsql nostr-db--connection
                       [:select [pubkey name display_name about picture nip05 lud16 updated_at]
                                :from profiles
                                :where (in pubkey $v1)]
                       (vconcat ids))))))

(defun nostr-db-select-relays ()
  "Return stored relay status rows."
  (emacsql nostr-db--connection
           [:select [url state message updated_at]
                    :from relay_status
                    :order-by [(asc url)]]))

(defun nostr-db--relay-preference-row-to-alist (row)
  "Convert relay preference ROW to an alist."
  (pcase-let ((`(,url ,marker ,read ,write) row))
    `((url . ,url)
      (marker . ,marker)
      (read . ,(not (zerop (or read 0))))
      (write . ,(not (zerop (or write 0)))))))

(defun nostr-db-select-relay-list (pubkey)
  "Return PUBKEY's NIP-65 relay list metadata."
  (mapcar #'nostr-db--relay-preference-row-to-alist
          (emacsql nostr-db--connection
                   [:select [url marker read write]
                            :from relay_preferences
                            :where (= pubkey $s1)
                            :order-by [(asc url) (asc marker)]]
                   pubkey)))

(provide 'nostr-db)
;;; nostr-db.el ends here
