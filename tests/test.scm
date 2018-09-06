#!/usr/bin/guile
!#

;; File: test.scm
;; Purpose: Run this file to run *all* tests written so far.

(load "test-macro.scm")
(load "test-from-file.scm")


;; Parse tests

(load "../new-code/parse.scm")
(load "parse-tests.scm")
(run-parse-tests #f)


;; Full tests

(load "../new-code/full-parse.scm")
(test-from-files
 "full-tests-input.txt"
 "full-tests-nofilter.txt"
 full-parse
 #f
 (fail-function "full parse")
 (pass-function "full parse"))