#!/usr/bin/guile
!#

(load "words.scm")
(load "pretty-print.scm")

;; File:    full-parse.scm
;; Purpose: Turn a string representing a composite form into a
;; list of the possible expansions, in pretty-print form.


;; Step one: Split a composite form into a list of serial forms.

(define (get-serials cf)
  (cond ((string? cf) ;; Single word
         (make-word cf))
        
        ;; MU-form
        ((eq? 'mu (car cf))
         (map (lambda (f) (list 'mu f))
              (get-serials (cadr cf))))

        ;; XY-form
        ((= 2 (length cf))
         (let ((head (get-serials (car cf)))
               (tail (get-serials (cadr cf))))
           (fold append
                 (map (lambda (h)
                        (map (lambda (t)
                               (list h t))
                             tail))
                      head))))

        ;; RU-form
        (#t
         (map (lambda (f)
                (cons (car cf) f))
              (get-serials (cdr cf))))))
          


(define (expand cf)
  (get-serials cf))

(define (full-parse str)
  (expand (parse str)))