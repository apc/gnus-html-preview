(defun gnus-preview-grab-html ()
  
  (let* ((buffer-string (buffer-substring (point-min) (point-max)))
	 (html-mimepart-preamble-location (string-match-p (regexp-quote "<#part type=text/html>") buffer-string))
	 (html-mimepart-postamble-location (string-match-p (regexp-quote "<#/multipart>") buffer-string)))
    (if html-mimepart-preamble-location
	(with-temp-buffer
	  "*ghost-buff*"
	  (insert buffer-string)
	  (buffer-substring (+ 23 html-mimepart-preamble-location) html-mimepart-postamble-location))
      (message "No HTML mimepart to preview"))))



(defun gnus-preview-write-html ()
  (interactive)
  (let ((html-to-write (gnus-preview-grab-html)))
    (if html-to-write

	(with-temp-file "/tmp/emailpreview.html"
	  (insert html-to-write)))))


(defun gnus-preview-current-email ()
  (interactive)
  (progn
    (gnus-preview-write-html)
    (browse-url "file:/tmp/emailpreview.html")))
