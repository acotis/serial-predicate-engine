#!/usr/bin/guile-1.8
!#


(load "stages/read.scm")
(load "stages/parse.scm")
(load "stages/interpret.scm")
(load "stages/render.scm")
(load "stages/pretty-print.scm")
(load "words.scm")

(use-modules (srfi srfi-1))


;; Each function calls the stage it represents, ensures the
;; output was suitable for the next stage (i.e., that it didn't
;; encounter any errors) then passes that output to the next
;; stage.

(define (present pp-output)
  (append
   (list "")
   (map (lambda (line n)
          (format #f "~a. ~a" n line))
        pp-output
        (cdr (iota (+ 1 (length pp-output)))))
   (list "")))
  
(define (safe-pp render-pass)
  (present (stage-pretty-print render-pass)))

(define (safe-render interpret-pass)
  (safe-pp (stage-render interpret-pass)))

(define (safe-interpret parse-pass)
  (safe-render (stage-interpret parse-pass)))

(define (safe-parse read-pass)
  (safe-interpret (stage-parse read-pass)))

(define (safe-read input)
  (let ((read-output (stage-read input)))
    (if (not (all-words? read-output))
        '("" "Sorry, I don't know all of those words." "")
        (safe-parse read-output))))


(define (full-parse input)
  (safe-read input))