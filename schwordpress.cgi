#!/bin/sh
#-*- scheme -*-

export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:`pwd`;
exec guile -s $0 2>/dev/null

!#

(define DATABASE          "schwordpress")
(define DATABASE-USER     "")
(define DATABASE-PASSWORD "")

(use-modules (srfi srfi-1)
	     (dbi dbi)
	     (sxml simple)
	     (www cgi)
	     (www server-utils answer))

(define (sxml->html xml)
  (with-output-to-string (lambda()(sxml->xml xml))))

;; Open connection to database.
(define cn (dbi-open
	    "mysql"
	    (string-join `(,DATABASE-USER
			   ,DATABASE-PASSWORD
			   ,DATABASE
			   "socket"
			   "/var/run/mysqld/mysqld.sock")
			 ":")))

(define (gather-posts cn limit)
  (dbi-query cn
	     (format #f "SELECT * FROM posts LIMIT ~a" limit))
  (let loop ((result '()))
    (let ((next (dbi-get_row cn)))
      (if next
	  (loop (cons next result))
	  (reverse result)))))

(define (post->paragraph post)
  `(div (@ (class "post"0))
	(div (@ (class "post-title"))             ,(assoc-ref post "title"))
	(div (@ (class "timestamp")) "Posted at " ,(assoc-ref post "timestamp"))
	(p (@ (class "content"))                  ,(assoc-ref post "content"))))

;; output.
(let ((mp (mouthpiece (current-output-port))))
  (mp #:set-reply-status:success)
  (mp #:add-header #:content-type "text/html")
  (mp #:add-content
      (sxml->html
       `(html
	 (head (title "It's Schwordpress!"))
	 (body
	  ,(map post->paragraph (gather-posts cn 999))))))
  (mp #:send-reply))
