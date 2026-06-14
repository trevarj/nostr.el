;;; nostr-media.el --- Inline media previews for Nostr buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Opt-in image loading for media placeholders rendered by `nostr-ui'.

;;; Code:

(require 'button)
(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)

(defvar url-http-end-of-headers)
(defvar url-request-method)

(defcustom nostr-media-cache-directory
  (expand-file-name "nostr-media/" user-emacs-directory)
  "Directory used to cache downloaded Nostr media."
  :type 'directory
  :group 'nostr)

(defcustom nostr-media-max-bytes (* 5 1024 1024)
  "Maximum image response size accepted by `nostr-media-load-at-point'."
  :type 'integer
  :group 'nostr)

(defcustom nostr-media-allowed-content-types
  '("image/png" "image/jpeg" "image/gif" "image/webp")
  "Content types allowed for inline Nostr image previews."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-media-auto-preview nil
  "Whether Nostr buffers may load inline media without an explicit command."
  :type 'boolean
  :group 'nostr)

(defcustom nostr-media-auto-preview-max-per-note 1
  "Maximum media URLs to automatically preview per note.
This is only used when `nostr-media-auto-preview' is non-nil.  Manual media
commands still operate on every supported media URL in the selected note."
  :type 'integer
  :group 'nostr)

(defcustom nostr-media-failure-cooldown 300
  "Seconds to suppress automatic retries for a media URL after fetch failure."
  :type 'integer
  :group 'nostr)

(defcustom nostr-media-inline-image-max-width 720
  "Maximum pixel width for inline media images."
  :type 'integer
  :group 'nostr)

(defcustom nostr-media-inline-image-max-height 480
  "Maximum pixel height for inline media images."
  :type 'integer
  :group 'nostr)

(defcustom nostr-media-video-player-command "mpv"
  "External command used to play video media URLs."
  :type 'string
  :group 'nostr)

(defcustom nostr-media-video-player-args '("--force-window=yes")
  "Arguments passed to `nostr-media-video-player-command' before the video URL."
  :type '(repeat string)
  :group 'nostr)

(defvar nostr-media-fetch-function nil
  "Optional test hook used to fetch media.
The function receives URL, SUCCESS and ERROR.  SUCCESS receives HEADERS and
binary DATA.  ERROR receives a human-readable message.")

(defvar nostr-media--in-flight (make-hash-table :test #'equal)
  "Media URLs currently being fetched, mapped to success callback lists.")

(defvar nostr-media--recent-failures (make-hash-table :test #'equal)
  "Recently failed media URLs, mapped to cons cells of time and message.")

(defun nostr-media--url-extension (url)
  "Return a cache file extension for URL."
  (let* ((parsed (url-generic-parse-url url))
         (filename (file-name-nondirectory (or (url-filename parsed) ""))))
    (if (string-match "\\.\\(png\\|jpe?g\\|gif\\|webp\\)\\(?:[?].*\\)?\\'" filename)
        (concat "." (downcase (match-string 1 filename)))
      ".img")))

(defun nostr-media-cache-file (url)
  "Return the cache file path for URL."
  (expand-file-name
   (concat (secure-hash 'sha1 url) (nostr-media--url-extension url))
   nostr-media-cache-directory))

(defun nostr-media--header (headers name)
  "Return case-insensitive header NAME from HEADERS alist."
  (alist-get name headers nil nil #'string-equal-ignore-case))

(defun nostr-media--validate-response (url headers data)
  "Validate downloaded DATA and HEADERS for URL."
  (let* ((content-type (car (split-string (or (nostr-media--header headers "content-type") "")
                                          ";" t "[ \t\n\r]+")))
         (content-length (nostr-media--header headers "content-length"))
         (declared-size (and content-length (string-to-number content-length)))
         (actual-size (string-bytes data)))
    (unless (member content-type nostr-media-allowed-content-types)
      (error "Unsupported media content type for %s: %s" url content-type))
    (when (and declared-size (> declared-size nostr-media-max-bytes))
      (error "Media exceeds size limit for %s: %s bytes" url declared-size))
    (when (> actual-size nostr-media-max-bytes)
      (error "Media exceeds size limit for %s: %s bytes" url actual-size))))

(defun nostr-media--validate-headers (url headers)
  "Validate response HEADERS for URL before downloading the body."
  (let* ((content-type (car (split-string (or (nostr-media--header headers "content-type") "")
                                          ";" t "[ \t\n\r]+")))
         (content-length (nostr-media--header headers "content-length"))
         (declared-size (and content-length (string-to-number content-length))))
    (unless (member content-type nostr-media-allowed-content-types)
      (error "Unsupported media content type for %s: %s" url content-type))
    (unless declared-size
      (error "Refusing %s: missing content-length" url))
    (when (> declared-size nostr-media-max-bytes)
      (error "Media exceeds size limit for %s: %s bytes" url declared-size))))

(defun nostr-media--write-cache (url headers data)
  "Validate and cache downloaded DATA for URL with HEADERS."
  (nostr-media--validate-response url headers data)
  (make-directory nostr-media-cache-directory t)
  (let ((file (nostr-media-cache-file url)))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert data))
    file))

(defun nostr-media--url-buffer-headers ()
  "Return HTTP headers from a `url-retrieve' buffer."
  (let (headers)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
        (push (cons "status" (match-string 1)) headers))
      (while (re-search-forward "^\\([^:\n]+\\):[ \t]*\\([^\n\r]+\\)" url-http-end-of-headers t)
        (push (cons (downcase (match-string-no-properties 1))
                    (string-trim (match-string-no-properties 2)))
              headers)))
    headers))

(defun nostr-media--fetch-body (url success error &optional fallback-headers)
  "Fetch URL body and call SUCCESS or ERROR."
  (url-retrieve
   url
   (lambda (status)
     (unwind-protect
         (if-let* ((err (plist-get status :error)))
             (funcall error (format "Could not fetch %s: %S" url err))
           (let ((headers (nostr-media--url-buffer-headers)))
             (if (not (equal (nostr-media--header headers "status") "200"))
                 (funcall error (format "Could not fetch %s: HTTP %s"
                                        url
                                        (or (nostr-media--header headers "status") "unknown")))
               (condition-case err
                   (let ((data (buffer-substring-no-properties
                                (1+ url-http-end-of-headers)
                                (point-max))))
                     (funcall success (append headers fallback-headers) data))
                 (error (funcall error (error-message-string err)))))))
       (kill-buffer (current-buffer))))
   nil t))

(defun nostr-media--fetch-head-then-body (url success error)
  "Validate URL with HEAD before downloading its body."
  (let ((url-request-method "HEAD"))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (funcall error (format "Could not fetch %s: %S" url err))
             (let ((headers (nostr-media--url-buffer-headers)))
               (if (not (equal (nostr-media--header headers "status") "200"))
                   (funcall error (format "Could not fetch %s: HTTP %s"
                                          url
                                          (or (nostr-media--header headers "status") "unknown")))
                 (condition-case err
                     (progn
                       (nostr-media--validate-headers url headers)
                       (nostr-media--fetch-body url success error headers))
                   (error (funcall error (error-message-string err)))))))
         (kill-buffer (current-buffer))))
     nil t)))

(defun nostr-media--failure-cooling-down-p (url)
  "Return non-nil when URL is still inside the media failure cooldown."
  (when-let* ((failure (gethash url nostr-media--recent-failures)))
    (< (- (float-time) (car failure))
       nostr-media-failure-cooldown)))

(defun nostr-media--remember-failure (url message)
  "Remember failed URL with MESSAGE for retry suppression."
  (puthash url (cons (float-time) message) nostr-media--recent-failures))

(defun nostr-media-fetch (url success error &optional force)
  "Fetch URL and call SUCCESS with headers/data or ERROR with a message.
When FORCE is non-nil, bypass recent failure suppression."
  (cond
   ((and (not force)
         (nostr-media--failure-cooling-down-p url))
    nil)
   ((gethash url nostr-media--in-flight)
    (puthash url
             (cons success (gethash url nostr-media--in-flight))
             nostr-media--in-flight)
    url)
   (t
    (puthash url (list success) nostr-media--in-flight)
    (let ((settled nil))
      (cl-labels
          ((finish-success
            (headers data)
            (unless settled
              (setq settled t)
              (let ((callbacks (gethash url nostr-media--in-flight)))
                (remhash url nostr-media--in-flight)
                (remhash url nostr-media--recent-failures)
                (dolist (callback callbacks)
                  (funcall callback headers data)))))
           (finish-error
            (message)
            (unless settled
              (setq settled t)
              (remhash url nostr-media--in-flight)
              (nostr-media--remember-failure url message)
              ;; One failed request may have many renderers waiting on it.
              ;; Report once; later automatic attempts are quiet during the
              ;; cooldown.
              (funcall error message))))
        (condition-case err
            (if nostr-media-fetch-function
                (funcall nostr-media-fetch-function
                         url #'finish-success #'finish-error)
              (nostr-media--fetch-head-then-body
               url #'finish-success #'finish-error))
          (error
           (finish-error (error-message-string err))))))
    url)))

(defun nostr-media-url-at-point ()
  "Return the media URL represented at point, when any."
  (or (get-text-property (point) 'nostr-media-url)
      (when-let* ((button (button-at (point))))
        (button-get button 'nostr-media-url))))

(defun nostr-media-type-at-point ()
  "Return the media type represented at point, when any."
  (or (get-text-property (point) 'nostr-media-type)
      (when-let* ((button (button-at (point))))
        (button-get button 'nostr-media-type))))

(defun nostr-media--available-image-width ()
  "Return a reasonable inline image width for the current window."
  (let ((window-width (when (fboundp 'window-pixel-width)
                        (ignore-errors (window-pixel-width)))))
    (if (and (integerp window-width) (> window-width 0))
        (min nostr-media-inline-image-max-width
             (max 160 (- window-width 64)))
      nostr-media-inline-image-max-width)))

(defun nostr-media--image-display-props ()
  "Return display props for constrained inline media images."
  (list :max-width (nostr-media--available-image-width)
        :max-height nostr-media-inline-image-max-height))

(defun nostr-media--image-string (url file)
  "Return display string for URL cached at FILE."
  (let ((label (format "[image loaded: %s]" url)))
    (if (and (display-images-p)
             (image-supported-file-p file))
        (propertize label
                    'display (apply #'create-image file nil nil
                                    (nostr-media--image-display-props))
                    'nostr-media-url url
                    'nostr-media-cache-file file)
      (propertize label
                  'nostr-media-url url
                  'nostr-media-cache-file file))))

(defun nostr-media--rendered-string (url file)
  "Return rendered media string for URL cached at FILE."
  (let ((text (concat "\n" (nostr-media--image-string url file))))
    (add-text-properties 0 (length text)
                         `(nostr-media-rendered t
                           nostr-media-url ,url
                           rear-nonsticky t)
                         text)
    (add-text-properties 0 1
                         '(nostr-media-render-start t)
                         text)
    (add-text-properties (1- (length text)) (length text)
                         '(nostr-media-render-end t)
                         text)
    text))

(defun nostr-media-play-video-url (url)
  "Play video URL with the configured player."
  (if-let* ((command (and (stringp nostr-media-video-player-command)
                          (not (string-empty-p nostr-media-video-player-command))
                          (executable-find nostr-media-video-player-command))))
      (let ((process (apply #'start-process
                            "nostr-media-video"
                            nil
                            command
                            (append nostr-media-video-player-args
                                    (list url)))))
        (set-process-query-on-exit-flag process nil)
        (message "Playing video with %s" nostr-media-video-player-command))
    (browse-url url)
    (message "Video player not found; opened video in browser"))
  url)

(defun nostr-media-play-video-at-point (&optional button)
  "Play the video URL represented at point or BUTTON with the configured player."
  (interactive)
  (let ((url (or (and button (button-get button 'nostr-media-url))
                 (nostr-media-url-at-point)
                 (user-error "No Nostr video URL at point"))))
    (nostr-media-play-video-url url)))

(defun nostr-media--rendered-block-end (pos limit)
  "Return end position for rendered preview block starting at POS before LIMIT."
  (let ((end (text-property-any pos limit 'nostr-media-render-end t)))
    (if end
        (1+ end)
      (next-single-property-change pos 'nostr-media-rendered nil limit))))

(defun nostr-media-remove-rendered-in-region (start end)
  "Remove rendered media previews between START and END.
Return the number of removed preview blocks."
  (let ((count 0)
        pos)
    (while (setq pos (text-property-any start end 'nostr-media-render-start t))
      (let ((next (nostr-media--rendered-block-end pos end)))
        (let ((inhibit-read-only t))
          (delete-region pos next)
          (setq end (- end (- next pos)))
          (setq count (1+ count)))))
    count))

(defun nostr-media-rendered-in-region-p (start end)
  "Return non-nil when rendered media exists between START and END."
  (let ((pos start)
        found)
    (while (and (not found) (< pos end))
      (setq found (get-text-property pos 'nostr-media-rendered))
      (setq pos (next-single-property-change pos 'nostr-media-rendered nil end)))
    found))

(defun nostr-media-remove-rendered-url-in-region (url start end)
  "Remove rendered preview for URL between START and END."
  (let (pos)
    (while (setq pos (text-property-any start end 'nostr-media-render-start t))
      (let ((next (nostr-media--rendered-block-end pos end)))
        (if (equal (get-text-property pos 'nostr-media-url) url)
            (let ((inhibit-read-only t))
              (delete-region pos next)
              (setq end (- end (- next pos))))
          (setq start next))))))

(defun nostr-media-render-file-at-point (url file &optional position)
  "Render cached media URL from FILE at POSITION or point."
  (let* ((marker (copy-marker (or position (point))))
         (buffer (marker-buffer marker)))
    (if (not (buffer-live-p buffer))
        (message "[nostr] Media buffer closed before %s could be rendered" url)
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          (end-of-line)
          (nostr-media-remove-rendered-url-in-region
           url
           (line-beginning-position)
           (save-excursion
             (forward-line 2)
             (point)))
          (let ((inhibit-read-only t))
            (insert (nostr-media--rendered-string url file))))))
    file))

;;;###autoload
(defun nostr-media-load-at-point (&optional cached-only force)
  "Load and render the media placeholder at point.
When CACHED-ONLY is non-nil, render only an existing cached file and do not
start a network request.  When FORCE is non-nil, bypass recent failure
suppression."
  (interactive)
  (let* ((url (or (nostr-media-url-at-point)
                  (user-error "No Nostr media placeholder at point")))
         (type (nostr-media-type-at-point))
         (file (nostr-media-cache-file url))
         (marker (copy-marker (point) t)))
    (cond
     ((eq type 'video)
      (nostr-media-play-video-url url))
     ((file-exists-p file)
        (nostr-media-render-file-at-point url file marker)
      url)
     (cached-only nil)
     (t
      (nostr-media-fetch
       url
       (lambda (headers data)
         (condition-case err
             (nostr-media-render-file-at-point
              url
              (nostr-media--write-cache url headers data)
             marker)
           (error (message "[nostr] %s" (error-message-string err)))))
       (lambda (message)
         (message "[nostr] %s" message))
       (or force (called-interactively-p 'interactive)))
      url))))

;;;###autoload
(defun nostr-media-open-at-point ()
  "Open the media URL at point externally."
  (interactive)
  (let ((url (or (nostr-media-url-at-point)
                 (user-error "No Nostr media URL at point"))))
    (browse-url url)
    url))

(provide 'nostr-media)
;;; nostr-media.el ends here
