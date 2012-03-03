(use-modules (srfi srfi-1))

(define is-content?
  (and-predicates (node-name=? 'p)
		  (attribute=? 'class "content")))

(define is-byline?
  (and-predicates (node-name=? 'div)
		  (attribute=? 'class "byline")))

(attach-to-hook!
 'post-post
 (lambda (post)
   (let* ((content (find-child-with-predicate post is-content?))
	  (text (apply string-append (filter string? content)))
	  (n (length (string-tokenize text))))
   (apply-to-relevant-node
    post
    is-byline?
    (insert-after-attributes
     (format #f "~a WORDS -- " n))))))


