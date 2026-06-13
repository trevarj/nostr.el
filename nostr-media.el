;;; nostr-media.el --- Inline media previews for Nostr buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Opt-in image loading for media placeholders rendered by `nostr-ui'.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)

(defvar url-http-end-of-headers)

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

(defvar nostr-media-fetch-function nil
  "Optional test hook used to fetch media.
The function receives URL, SUCCESS and ERROR.  SUCCESS receives HEADERS and
binary DATA.  ERROR receives a human-readable message.")

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

(defun nostr-media-fetch (url success error)
  "Fetch URL and call SUCCESS with headers/data or ERROR with a message."
  (if nostr-media-fetch-function
      (funcall nostr-media-fetch-function url success error)
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
                 (let ((data (buffer-substring-no-properties
                              (1+ url-http-end-of-headers)
                              (point-max))))
                   (funcall success headers data)))))
         (kill-buffer (current-buffer))))
     nil t)))

(defun nostr-media-url-at-point ()
  "Return the media URL represented at point, when any."
  (or (get-text-property (point) 'nostr-media-url)
      (when-let* ((button (button-at (point))))
        (button-get button 'nostr-media-url))))

(defun nostr-media--image-string (url file)
  "Return display string for URL cached at FILE."
  (let ((label (format "[image loaded: %s]" url)))
    (if (and (display-images-p)
             (image-supported-file-p file))
        (propertize label
                    'display (create-image file)
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
  (let ((marker (copy-marker (or position (point)))))
    (with-current-buffer (marker-buffer marker)
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
          (insert (nostr-media--rendered-string url file)))))
    file))

;;;###autoload
(defun nostr-media-load-at-point ()
  "Load and render the media placeholder at point."
  (interactive)
  (let* ((url (or (nostr-media-url-at-point)
                  (user-error "No Nostr media placeholder at point")))
         (file (nostr-media-cache-file url))
         (marker (copy-marker (point) t)))
    (if (file-exists-p file)
        (nostr-media-render-file-at-point url file marker)
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
         (message "[nostr] %s" message))))
    url))

(provide 'nostr-media)
;;; nostr-media.el ends here
