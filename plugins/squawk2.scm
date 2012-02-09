(use-modules (srfi srfi-1))

(define (apply-to-relevant-node root predicate op)
  (if (predicate root)
      (op root)
      (map (lambda (node)
	     (if (list? node)
		 (apply-to-relevant-node node predicate op)
		 node))
	   root)))

(define (find-child-with-name root name)
  (find (lambda (x)(and (list? x)(equal? (car x) name))) root))

(define (is-content? node)
  (and (equal? (car node) 'p)
       (let ((attrs (find-child-with-name node '@)))
	 (and attrs (let ((class (find-child-with-name attrs 'class)))
		      (equal? (cadr class) "content"))))))

(attach-to-hook!
 'post-post
 (lambda (post)
   (apply-to-relevant-node
    post
    is-content?
    (lambda (node)
      (with-output-to-file
	  "/tmp/content.txt"
	(lambda ()(display node)(newline)))))
   post))