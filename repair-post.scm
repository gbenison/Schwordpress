
;
; GCB 5apr12
;
; These utilities will "repair" a broken Schwordpress post by replacing the 'content' field
; with its Scheme-quoted version.
;
; This can be used for example to grandfather in older post content that was not strictly formatted
; as an sexp, but contains characters e.g. () that interfere with sexp parsing.
;

;; Open connection to database.
(define DATABASE          "schwordpress")
(define DATABASE-USER     "")
(define DATABASE-PASSWORD "")

(define cn (dbi-open
	    "mysql"
	    (string-join `(,DATABASE-USER
			   ,DATABASE-PASSWORD
			   ,DATABASE
			   "socket"
			   "/var/run/mysqld/mysqld.sock")
			 ":")))

(define (get-content cn id)
  (dbi-query cn (format #f "SELECT content from posts where id=~a" id))
  (assoc-ref (dbi-get_row cn) "content"))

(define (repair-post cn id)
		       (let* ((content (get-content cn id))
			      (repaired (format #f "~s" content)))
			 (dbi-query cn (format #f "UPDATE posts SET content=~s WHERE id=~a" repaired id))))


