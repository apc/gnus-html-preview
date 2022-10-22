;; gnus-html-preview.el --- Preview HTML mimepart of a message

;;; Commentary
;; In Emacs, when composing an email in message mode, get a quick and
;; dirty preview of the mimepart in the default system browser.

;;; Code:

(defun gnus-html-preview-needle-haystack (needle haystack)
  "Find location of string NEEDLE in string HAYSTACK."
  (string-match-p (regexp-quote needle) haystack))


(defun gnus-html-preview-replace-line-breaks (string)
  "Replaces all line breaks in STRING with HTML <br> tags."
  (replace-regexp-in-string "\n" "<br>" string))


(defun gnus-html-preview-escape-angle-brackets (string)
  "Escape all angle brackets in STRING."
  (replace-regexp-in-string ">" "&gt" (replace-regexp-in-string "<" "&lt" string)))


(defun gnus-html-preview-grab-segment (string-begin string-end &optional shift-begin)
  "Get string from buffer that begins with STRING-BEGIN and ends with STRING-END."
  (let* ((buffer-string (buffer-substring (point-min) (point-max)))
	 (html-mimepart-preamble-location (gnus-html-preview-needle-haystack string-begin buffer-string))
	 (html-mimepart-postamble-location (gnus-html-preview-needle-haystack string-end buffer-string))
	 (shift (if shift-begin shift-begin 0)))
    (if html-mimepart-preamble-location
	(with-temp-buffer
	  "**ghost-buff**"
	  (insert buffer-string)
	  (buffer-substring (+ shift html-mimepart-preamble-location) html-mimepart-postamble-location))
      (message "No HTML mimepart to preview"))))


(defun gnus-html-preview-grab-html ()
  "Get the HTML mimepart from current message buffer."
  (gnus-html-preview-grab-segment "<#part type=text/html>" "<#/multipart>" 23))


(defun gnus-html-preview-grab-heading ()
  "Grab some info from the beginning of the email to display along with the preview."
  (gnus-html-preview-grab-segment "To:" "--text follows this line--" 1))


(defun gnus-html-preview-write ()
  (interactive)
  (let ((html (gnus-html-preview-grab-html))
	(heading (gnus-html-preview-grab-heading)))
    (if html
	(with-temp-file "/tmp/emailpreview.html"
	  (insert "<html><head><title>Email Preview</title><style>.indented {margin: 0 10% 0 10%}</style></head><body><pre class='indented'>")
	  (insert (gnus-html-preview-replace-line-breaks (gnus-html-preview-escape-angle-brackets heading)))
	  (insert "</pre>")
	  (insert "<hr><main class='indented'>")
	  (insert html)
	  (insert "</main></body></html>")))))




(defun gnus-html-preview-current-email ()
  (interactive)
  (progn
    (gnus-html-preview-write)
    (browse-url "file:/tmp/emailpreview.html")))

(provide 'gnus-html-preview)

;;; gnus-html-preview.el ends here
