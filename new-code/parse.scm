#!/usr/bin/guile
!#

;; File:    parse.scm
;; Purpose: Translate a string representing a serial predicate
;;          into a parsed-composite structure (see doc file
;;          "new-structure.odt" for more info).

(load "utilities.scm")
(use-modules (srfi srfi-1))


;; Members of RU

(define RU '(ru ra ro ri re roi))
(define cmavo (append RU '(to mu)))


;; Preprocessing

(define (split-on-spaces string)
  (remove (lambda (s) (equal? s ""))
          (string-split string #\Space)))

(define (replace-cmavo-strings com)
  (map (lambda (s)
         (let ((sym (string->symbol s)))
           (if (member sym cmavo) sym s)))
       com))


;; Parse a composite into a parse-form
;; i.e. ("jai") -> "jai"
;;      (to ru "kuai" to "tua") -> (ru "kuai" "tua")

(define (parse-composite com)
  (cond ((= (length com) 1)
         (car com))

        ((every string? com)
         (fold-right list com))
         
        (#t '())))


;; Full parse function

(define (parse string)
  (parse-composite
   (replace-cmavo-strings
    (split-on-spaces string))))