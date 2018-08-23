#!/usr/bin/guile
!#


;; File:    full-parse.scm
;; Purpose: Turn a string representing a composite form into a
;; list of the possible expansions, in pretty-print form.


(define (expand pc)
  (make-list 100 "not-yet-implemented"))

(define (full-parse str)
  (expand (parse str)))