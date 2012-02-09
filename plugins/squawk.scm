(use-modules (srfi srfi-19))

(with-output-to-file "/tmp/squawk.txt"
  (lambda ()
     (display "hello, world!\n")
     (display (date->string (current-date)))
     (newline)))

