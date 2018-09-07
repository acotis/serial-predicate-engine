#!/usr/bin/guile
!#


(load "full-parse.scm")
(use-modules (ice-9 readline))


(define (repl)
  (format #t "~%> ")
  (force-output)

  (let ((input (readline)))
    (if (not (equal? "q" input))
        
        (let ((parse (full-parse input)))
          (map (lambda (line n)
                 (format #t "~a. ~a~%" n line))
               parse
               (cdr (iota (+ 1 (length parse)))))
          (repl)))))


(format #t "Enter any Toaq serial predicate to have it parsed.~%Enter q to quit.~%")
(repl)