#!/usr/bin/guile
!#

;; File: test.scm
;; Purpose: Run this file to run *all* tests written so far.

(load "test-macro.scm")
(load "test-from-file.scm")


;; Parse tests

;; (load "../new-code/full-parse.scm")
;; (load "parse-tests.scm")
;; (run-parse-tests #f)


;; Full tests

(load "../code/full-parse.scm")
(test-from-files
 "./tests/full-tests-input.txt"
 "./tests/full-tests-full-only.txt"
 full-parse
 #f
 (fail-function "full parse (full-form only)")
 (pass-function "full parse (full-form only)"))