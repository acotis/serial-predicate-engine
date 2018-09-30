#!/usr/bin/guile
!#


(load "full-parse.scm")
(load "words.scm")
(use-modules (ice-9 readline))


;; Read eval print loop

(define (repl)
  (format #t "> ") ;; Prompt
  (force-output)

  ;; Processing
  (let ((input (readline)))
    (if (not (equal? "q" input))      
        (begin
          (catch #t
                 (lambda ()
                   (map (lambda (line)
                          (format #t "~a~%" line))
                        (full-parse input)))
                   
                 (lambda (key . param)
                   (format #t "~%Sorry, that input caused an error:~%  1. ~a~%  2. ~a~%~%" key param)))

          (repl)))))


;; Load the words

(load-words)

;; Print the introduction and begin the loop

(format #t "Enter any Toaq serial predicate to parse it.~%")
(format #t "Enter only the words, not the tones.~%")
(format #t "Example: to ri pai ru jai to mu mai~%")
(format #t "Enter q to quit.~%")
(format #t "~%")
(repl)