(use-modules (srfi srfi-1))

(define (is-content? node)
  (and (equal? (car node) 'p)
       (let ((attrs (find-child-with-name node '@)))
	 (and attrs (let ((class (find-child-with-name attrs 'class)))
		      (equal? (cadr class) "content"))))))

(attach-to-hook!
 'post-post
 (lambda (post)
   (let* ((content (find-child-with-predicate post is-content?))
	  (text (apply string-append (filter string? content)))
	  (n (length (string-tokenize text))))
   (apply-to-relevant-node
    post
    is-content?
    (insert-after-attributes
     (format #f "length ~a -- " n))))))


