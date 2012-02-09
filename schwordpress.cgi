#!/bin/sh
#-*- scheme -*-

export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:`pwd`;
export GUILE_WARN_DEPRECATED=no;
exec guile -s $0 2>/dev/null

!#

(use-modules (srfi srfi-1)
	     (srfi srfi-19)
	     (ice-9 regex)
	     (dbi dbi)
	     (sxml simple)
	     (www cgi)
	     (www server-utils answer))

; --------- parameters ------------
(define DATABASE          "schwordpress")
(define DATABASE-USER     "")
(define DATABASE-PASSWORD "")

(define blog-name "Schwordpress Demo")
(define css-file-name "schwordpress-standard.css")

; ----------- utilities --------------

; functions lacking side effects, whose behavior is unlikely to be altered
; by plugins.

(define log
  (let ((log-file (open-output-file "/tmp/schwordpress.log")))
    (lambda args (map (lambda (x)
			(display x log-file)
			(display " " log-file)
			(newline log-file))
		      args))))

(define (sxml->html xml)
  (with-output-to-string (lambda()(sxml->xml xml))))

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

(define (find-child-with-name root name)
  (find (lambda (x)(and (list? x)(equal? (car x) name))) root))

(define (find-child-with-predicate root predicate)
  (if (predicate root)
      root
      (find (lambda (child)
	      (find-child-with-predicate child predicate))
	    (filter list? root))))

;; Echos 'root', except recursively tests children for
;; (predicate node) == #t, and if so, replaces node
;; with (op node).
(define (apply-to-relevant-node root predicate op)
  (if (predicate root)
      (op root)
      (map (lambda (node)
	     (if (list? node)
		 (apply-to-relevant-node node predicate op)
		 node))
	   root)))

;; Returns a function(node) that inserts "content" after the attributes
;; element, or immediately after the name if no attributes element found.
(define (insert-after-attributes content)
  (lambda (node)
    (cond ((null? (cdr node))
	   (cons (car node)(cons content (cdr node))))
	  ((and (list? (cadr node))
		(equal? '@ (caadr node)))
	   (cons (car node)
		 (cons (cadr node)
		       (cons content (cddr node)))))
	  (else (cons (car node)(cons content (cdr node)))))))

(define (get-attribute node name)
  (let ((attrs (find-child-with-name node '@)))
    (and attrs (let ((attr (find-child-with-name attrs name)))
		 (and attr
		      (if (null? (cdr attr))
			  #t
			  (cadr attr)))))))

(define (attribute-predicate attribute-name op)
  (lambda (node)
    (let ((attr (get-attribute node attribute-name)))
      (and attr (op attr)))))

(define (attribute=? key value)
  (attribute-predicate key (lambda (x)(equal? x value))))

(define (node-name=? name)
  (lambda (node)(and (list? node)
		     (not (null? node))
		     (equal? (car node) name))))

(define (and-predicates . predicates)
  (lambda (node)
    (fold (lambda (op init)
	    (and (op node) init)) #t predicates)))

; ------------ hooks --------------------------------

(define %hooks% '())

(define (attach-to-hook! name op)
  (set! %hooks% (assoc-set! %hooks% name
			    (cons op (or (assoc-ref %hooks% name) '())))))

(define (run-hooks name init)
  (fold
   (lambda (op init)(op init))
   init
   (or (assoc-ref %hooks% name) '())))

; ------------ cgi and database initialization ------

(cgi:init)

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

; ------------- load plugins -------------
;
; All available plugins reside in "plugins".
; An "enabled" plugin residues in "plugins/enabled".
; The usual way to put it there is to create a symlink.
;

(let ((dir-stream (opendir "plugins/enabled")))
  (let loop ()
    (let ((next (readdir dir-stream)))
      (if (not (eof-object? next))
	  (begin
	    (if (string-match ".scm$" next)
		(let ((fname (string-append (getcwd) "/plugins/enabled/" next)))
		  (log "plugin:" fname)
		  (load fname)))
	    (loop))))))

; ------------- content generation -------

(define (gather-posts cn limit)
  (dbi-query cn
	     (format #f "SELECT id,title,timestamp,content FROM posts ORDER BY timestamp DESC LIMIT ~a" limit))
  (let loop ((result '()))
    (let ((next (dbi-get_row cn)))
      (if next
	  (loop (cons next result))
	  (reverse result)))))

(define (format-timestamp timestamp)
  (date->string
   (string->date timestamp "~Y-~m-~d~H:~M")
   "Posted at ~H:~M on ~A, ~B ~d ~Y"))

(define (post->paragraph post)
  (run-hooks
   'post-post
   `(div (@ (class "post"))
	 (div (@ (class "post-title"))    ,(assoc-ref post "title"))
	 (div (@ (class "timestamp"))     ,(format-timestamp (assoc-ref post "timestamp")))
	 (p (@ (class "content"))         ,(assoc-ref post "content"))
	 (div (@ (class "meta"))
	      (a (@ (href ,(format #f "schwordpress.cgi?request=delete&id=~a" (assoc-ref post "id"))))
		 "DELETE POST")))))

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
     ,(run-hooks
       'with-header
       `(div (@ (class "header"))
	     (a (@ (href "schwordpress.cgi")(id "blog-title"))
		(h1 ,blog-name))))
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
		"INSERT INTO posts (title, timestamp, content) VALUES (~a, now(), ~a)"
		(->string title)
		(->string content))))
	 (dbi-query cn query)))
   (standard-page-with-content
   '(p (a (@ (id "new-post-button")
	     (href "schwordpress.cgi?request=new-post"))
	  "NEW POST"))
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

(define (delete-requested-post)
  (let* ((post-id (assoc-ref cgi:query-string 'id)) ;; FIXME handle error gracefully
	 (query (format #f "DELETE FROM posts WHERE id=~a LIMIT 1" post-id)))
    (dbi-query cn query)))

;; output.
(let ((mp (mouthpiece (current-output-port))))
  (mp #:set-reply-status:success)
  (mp #:add-header #:content-type "text/html")
  (mp #:add-content
      (sxml->html
       (case (->symbol cgi:request)
	 ((new-post)  (new-post))
	 ((delete)
	  (delete-requested-post)
	  (main-page))
	 ((#f)        (main-page))
	 (else "** error: unknown request **"))))
  (mp #:send-reply))
