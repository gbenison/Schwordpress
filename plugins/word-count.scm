(use-modules (srfi srfi-1))

(define is-content?
  (and-predicates (node-name=? 'p)
		  (attribute=? 'class "content")))

(define is-timestamp?
  (and-predicates (node-name=? 'div)
		  (attribute=? 'class "timestamp")))

(attach-to-hook!
 'post-post
 (lambda (post)
   (let* ((content (find-child-with-predicate post is-content?))
	  (text (apply string-append (filter string? content)))
	  (n (length (string-tokenize text))))
   (apply-to-relevant-node
    post
    is-timestamp?
    (insert-after-attributes
     (format #f "~a WORDS -- " n))))))


