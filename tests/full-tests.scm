#!/usr/bin/guile
!#


(load "test-from-file.scm")


(define (full-parse str)
  (make-list 100 str))


(test-from-files
 "full-tests-input.txt"
 "full-tests-nofilter.txt"
 full-parse
 #t
 (fail-function "full parse")
 (pass-function "full parse"))