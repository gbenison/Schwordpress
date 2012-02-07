#!/bin/sh
#-*- scheme -*-

export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:`pwd`;
export GUILE_WARN_DEPRECATED=no;
exec guile -s $0 2>/dev/null

!#

(define DATABASE          "schwordpress")
(define DATABASE-USER     "")
(define DATABASE-PASSWORD "")

(define blog-name "Schwordpress Demo")
(define css-file-name "schwordpress-standard.css")

(use-modules (srfi srfi-1)
	     (dbi dbi)
	     (sxml simple)
	     (www cgi)
	     (www server-utils answer))

(define (sxml->html xml)
  (with-output-to-string (lambda()(sxml->xml xml))))

(cgi:init)

(define (->symbol x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	((not x) #f)
	(else (string->symbol (format #f "~a" x)))))

(define (query->pair query)
  (let ((tmp (string-split query #\=)))
    (if (= (length tmp) 2)
	(cons (string->symbol (car tmp))(cadr tmp))
	(cons 'na "NA"))))

(define cgi:query-string (map query->pair (string-split (cgi:getenv 'query-string) #\&)))
(define cgi:request (assoc-ref cgi:query-string 'request))

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
  `(div (@ (class "post"))
	(div (@ (class "post-title"))             ,(assoc-ref post "title"))
	(div (@ (class "timestamp")) "Posted at " ,(assoc-ref post "timestamp"))
	(p (@ (class "content"))                  ,(assoc-ref post "content"))))

(define (standard-page-with-content . content)
  `(html
    (head
     (title "It's Schwordpress!")
     ,(if css-file-name
	  `(link (@ (rel "stylesheet")
		    (href ,css-file-name)
		    (type "text/css")))
	  '()))
    (body
     (a (@ (href "schwordpress.cgi"))
	(h1 ,blog-name))
     ,content)))

(define (->string x)
  (with-output-to-string (lambda ()(write x))))

(define (main-page)
   (if (member "new-post-title" (cgi:names))
       (let* ((title (car (cgi:values "new-post-title")))
	      (content (car (cgi:values "new-post-content")))
	      (query
	       (format
		#f
		"INSERT INTO posts VALUES (~a, now(), ~a)"
		(->string title)
		(->string content))))
	 (with-output-to-file "/tmp/query.txt" (lambda ()(display query)))
	 (dbi-query cn query)))
   (standard-page-with-content
   '(a (@ (href "schwordpress.cgi?request=new-post")) "New post")
   (map post->paragraph (gather-posts cn 999))))

;; FIXME validate input
(define (new-post)
  (standard-page-with-content
   `((h2 "New post")
     (form (@ (method "POST")
	      (action "schwordpress.cgi")
	      (name "new-post"))
	   (div (@ (id "new-post-title"))
		"Title"
		(input (@ (type "text")
			  (name "new-post-title"))))
	   (div (@ (id "new-post-content"))
		(textarea (@ (name "new-post-content")
			     (rows 20)
			     (cols 60))
			  "- Enter new post content here -"))
	   (input (@ (type "submit")
		     (value "POST")))))))


;; output.
(let ((mp (mouthpiece (current-output-port))))
  (mp #:set-reply-status:success)
  (mp #:add-header #:content-type "text/html")
  (mp #:add-content
      (sxml->html
       (case (->symbol cgi:request)
	 ((new-post)  (new-post))
	 ((#f)        (main-page))
	 (else "** error: unknown request **"))))
  (mp #:send-reply))
