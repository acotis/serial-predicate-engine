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


(define debug #f)


;; Each function calls the stage it represents, ensures the
;; output was suitable for the next stage (i.e., that it didn't
;; encounter any errors) then passes that output to the next
;; stage.

(define (present pp-output)
  (if debug
      (begin
        (format #t "(present ~a)~%" pp-output)
        (format #t "  [char string] ~a~%" (string->list (car pp-output)))))
  
  (append
   (list "")
   pp-output
   (list "")))


(define (safe-pp render-pass)
  (if debug (format #t "(safe-pp ~a)~%" render-pass))
  (present (stage-pretty-print render-pass)))


(define (safe-render interpret-pass)
  (if debug (format #t "(safe-render ~a)~%"interpret-pass))
  (safe-pp (stage-render interpret-pass)))


(define (safe-interpret parse-pass)
  (if debug (format #t "(safe-interpret ~a)~%" parse-pass))
  (safe-render (stage-interpret parse-pass)))


(define (safe-parse read-pass)
  (if debug (format #t "(safe-parse ~a)~%" read-pass))
  (safe-interpret (stage-parse read-pass)))


(define (safe-read input)
  (if debug (format #t "(safe-read \"~a\")~%" input))
  
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