#!/bin/sh
#-*- scheme -*-
#
#    Copyright (C) 2012 Greg Benison
#   
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#   
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#   
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#  

export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:`pwd`;
export GUILE_WARN_DEPRECATED=no;
# exec guile -s $0 2>/dev/null
exec guile -s $0 2>>guile-error.log

!#

(use-modules (srfi srfi-1)
	     (srfi srfi-19)
	     (dbi dbi)
	     (www session db)
	     (sxml simple)
	     (www cgi)
	     (www server-utils answer))

(define DATABASE          "schwordpress")
(define DATABASE-USER     "")
(define DATABASE-PASSWORD "")

(define blog-name "Schwordpress Demo")
(define css-file-name "schwordpress-standard.css")

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

(define session
  (session:db
   (lambda (query)(dbi-query cn query)(dbi-get_row cn)) "sessions"))

(define (gather-posts cn limit)
  (dbi-query cn
	     (format #f "SELECT id,title,timestamp,content,author FROM posts ORDER BY timestamp DESC LIMIT ~a" limit))
  (let loop ((result '()))
    (let ((next (dbi-get_row cn)))
      (if next
	  (loop (cons next result))
	  (reverse result)))))

(define (format-byline post)
  (string-append
   "Posted by "
   (assoc-ref post "author")
   " at "
   (date->string
    (string->date (assoc-ref post "timestamp") "~Y-~m-~d~H:~M")
    " ~H:~M on ~A, ~B ~d ~Y")))

(define (post->paragraph post)
  `(div (@ (class "post"))
	(div (@ (class "post-title"))    ,(assoc-ref post "title"))
	(div (@ (class "byline"))        ,(format-byline post))
	(p (@ (class "content"))         ,(assoc-ref post "content"))
	(div (@ (class "meta"))
	     ,(if (session-get-user session)
		  `(a (@ (href ,(format #f "schwordpress.cgi?request=delete&id=~a" (assoc-ref post "id"))))
		      "Delete post")
		  ""))))

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
     (div (@ (id "header"))
	  (a (@ (href "schwordpress.cgi")(id "blog-title"))
	     (h1 ,blog-name)))
     (div (@ (id "container"))
	  (div (@ (id "content"))
	       ,content)
	  (div (@ (id "sidebar"))
	       (div (@ (id "meta"))
		    ,(let ((user (session-get-user session)))
		       (if user
			   `(ul
			     (li (span "Welcome, " ,user))
			     (li (a (@ (href "schwordpress.cgi?request=login"))
				    "Log in as a different user"))
			     (li (a
				  (@ (href "schwordpress.cgi?request=logout"))
				  "Log out")))
			    `(ul
			      (li (span "Not logged in"))
			      (li (a (@ (href "schwordpress.cgi?request=login"))
				     "Log in"))))))
	       (p (a (@ (id "new-post-button")
			(href "schwordpress.cgi?request=new-post"))
		     "NEW POST")))))))

(define (->string x)
  (with-output-to-string (lambda ()(write x))))

(define (login-form)
  `(form (@ (id "login")
	    (method "post")
	    (action "schwordpress.cgi?request=post-login"))
	 (div (@ (id "uname"))
	      (span "User name:")
	      (input (@ (type "text")
			(name "uname"))))
	 (div (@ (id "passwd"))
	      (span "Password:")
	      (input (@ (type "password")
			(name "password"))))
	 (input (@ (type "submit")(value "Log in")))))

(define (login-page)
  (standard-page-with-content (login-form)))

(define (invalid-login-page)
  (standard-page-with-content
   `((div (@ (class "invalid_login_warning"))
	  (span "Invalid user name and/or password"))
     ,(login-form))))

(define (with-login-required content)
  (if (session-get-user session)
      content
      `((div (@ (class "login_required_warning"))
	     (span "You must be logged in to access this function."))
	,(login-form))))

(define (safe-car xs)(false-if-exception (car xs)))

(define (process-login)
  (let ((uname  (safe-car (cgi:values "uname")))
	(passwd (safe-car (cgi:values "password"))))
    (dbi-query
     cn
     (format #f "SELECT * FROM users WHERE uname='~a' and password='~a'"
	     uname passwd))
    (and (dbi-get_row cn)
	 (session-set-user! session uname))))
   
(define (main-page)
   (if (member "new-post-title" (cgi:names))
       (let* ((title (car (cgi:values "new-post-title")))
	      (content (car (cgi:values "new-post-content")))
	      (query
	       (format
		#f
		"INSERT INTO posts (title, timestamp, content, author) VALUES (~a, now(), ~a, ~a)"
		(->string title)
		(->string content)
		(->string (session-get-user session)))))
	 (dbi-query cn query)))
   (standard-page-with-content
    (map post->paragraph (gather-posts cn 999))))

(define (new-post)
  (standard-page-with-content
   (with-login-required
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
		      (value "POST"))))))))

(define (delete-requested-post)
  (if (session-get-user session) ;; login required.
      (let* ((post-id (assoc-ref cgi:query-string 'id)) ;; FIXME handle error gracefully
	     (query (format #f "DELETE FROM posts WHERE id=~a LIMIT 1" post-id)))
	(dbi-query cn query))))

;; output.
(let ((mp (mouthpiece (current-output-port))))
  (mp #:add-header #f (session-propagate session))
  (mp #:add-content
      (sxml->html
       (case (->symbol cgi:request)
	 ((new-post)  (new-post))
	 ((delete)
	  (delete-requested-post)
	  (main-page))
	 ((login)     (login-page))
	 ((post-login)
	  (if (process-login)
	      (main-page)
	      (invalid-login-page)))
	 ((logout)
	  (session-set-user! session #f)
	  (main-page))
	 ((#f)        (main-page))
	 (else "** error: unknown request **"))))
  (mp #:set-reply-status:success)
  (mp #:send-reply))



