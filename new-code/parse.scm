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

(define (is-RU? e)
  (member e RU))


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
  (cond (
         ;; Ignore all cases containing TO for now
          (member 'to com)
          '())

         ;; Ignore all cases containing MU for now
         ((member 'mu com)
          '())
         
         ;; Ignore all cases containing members of RU for now
         ((any is-RU? com)
          '())

         ;; com contains only parse-forms

         ;; One parse-form
         ((= 1 (length com))
          (car com))

         ;; More than one parse-form
         (#t
          (list (car com)
                (parse-composite (cdr com))))))
         
;; Full parse function

(define (parse string)
  (parse-composite
   (replace-cmavo-strings
    (split-on-spaces string))))