#!/usr/bin/guile-1.8
!#


(load "stages/read.scm")
(load "stages/parse.scm")
(load "stages/interpret.scm")
(load "stages/render.scm")
(load "stages/pretty-print.scm")

(load "utilities.scm")
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
  (format #t "(safe-pp ...)~%")
  (present (stage-pretty-print render-pass)))

(define (safe-render interpret-pass)
  (format #t "(safe-render ~a)~%" interpret-pass)
  (safe-pp (stage-render interpret-pass)))

(define (safe-interpret parse-pass)
  (format #t "(safe-interpret ...)~%")
  (safe-render (stage-interpret parse-pass)))

(define (safe-parse read-pass)
  (format #t "(safe-parse ...)~%")
  (safe-interpret (stage-parse read-pass)))

(define (safe-read input)
  (format #t "(safe-read ...)~%")

  (let* ((read-output (stage-read input))
         (unknown (get-unknown-words read-output)))
    
    (if (not (equal? '() unknown))
        `(""
          ,(fold string-append
                (cons "Sorry, I don't know these words: "
                      (map (lambda (word)
                             (format #f "\"~a\" " word))
                           unknown)))
          "")
            
        (safe-parse read-output))))


(define (full-parse input)
  (safe-read input))