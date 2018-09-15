#!/usr/bin/guile
!#


(load "full-parse.scm")
(use-modules (ice-9 readline))


;; Eval print

(define (ep input)
  (let ((parse (full-parse input)))
    (format #t "~%")
    (map (lambda (line n)
           (format #t "~a. ~a~%" n line))
         parse
         (cdr (iota (+ 1 (length parse)))))))


;; Read eval print loop

(define (repl)
  (format #t "~%> ") ;; Prompt
  (force-output)

  ;; Processing
  (let ((input (readline)))
    (if (not (equal? "q" input))      
        (begin
          (catch #t
                 (lambda ()
                   (ep input))  ;; <- Real stuff happens here
                   
                 (lambda (key . param)
                   (format #t "~%Sorry, that input caused an error:~%  1. ~a~%  2. ~a~%" key param)))

          (repl)))))


(format #t "Enter any Toaq serial predicate to parse it.~%")
(format #t "Enter only the words, not the tones.~%")
(format #t "Example: to ri pai ru jai to mu mai~%")
(format #t "Enter q to quit.~%")
(repl)