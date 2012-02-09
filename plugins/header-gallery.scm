;; Select a random image from "content/images" and paste into the
;; page header.

(define my-random
  (let ((stream (open-input-file "/dev/urandom")))
    (lambda (x)
     (remainder (char->integer (read-char stream)) x))))

(define image-dir "content/images")

;; FIXME put in the actual image!!
(attach-to-hook!
 'with-header
 (lambda (header)
   (let* ((image-files
	   (filter
	    (string-match-pred ".png$")
	    (dir->files image-dir)))
	  (image-fname (list-ref image-files (my-random (length image-files))))
	  (new-header
	   (append header
		   `((img (@ (src ,(string-append image-dir "/" image-fname))))))))
     (log new-header)
     new-header)))


