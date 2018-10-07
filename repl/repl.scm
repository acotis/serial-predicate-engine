#!/usr/bin/guile
!#


(load "../api/api.scm")
(use-modules (ice-9 readline))

;; Necessary to print unicode characters properly
;; Note: This procedure of setting the port encoding correctly
;;       seems a bit delicate.  Be careful.
(set-port-encoding! (current-output-port) "UTF-8")


;; Read eval print loop

(define (repl)
  (format #t "> ") ;; Prompt

  ;; Processing
  (let ((input (readline)))
    (if (not (equal? "q" input))      
        (begin
          (catch #t
                 (lambda ()
                   (map (lambda (line)
                          (format #t "~a~%" line))
                        (api-parse input)))
                   
                 (lambda (key . param)
                   (format #t "~%Sorry, that input caused an error:~%  1. ~a~%  2. ~a~%~%" key param)))

          (repl)))))


;; Call the preload function

(api-preload)

;; Print the introduction and begin the loop

(format #t "Enter any Toaq serial predicate to parse it.~%")
(format #t "Enter only the words, not the tones.~%")
(format #t "Example: to ri pai ru jai to mu mai~%")
(format #t "Enter q to quit.~%")
(format #t "~%")
(repl)