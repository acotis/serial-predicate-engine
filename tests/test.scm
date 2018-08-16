#!/usr/bin/guile
!#

;; File: test.scm
;; Purpose: Run this file to run *all* tests written so far.

(load "test-macro.scm")

(run-tests
 (( (+ 3 5) 8 )
  ( (+ 4 1) 1 ))

 #t

 (lambda (r) (format #t "~a/~a tests failed~%"
                     (car r) (+ (car r) (cdr r))))
 (lambda (r) (format #t "okay~%")))


;(load "../new-code/parse.scm")
;(load "parse-tests.scm")
;(run-parse-tests #t)