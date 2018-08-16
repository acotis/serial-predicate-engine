#!/usr/bin/guile
!#

;; File: test.scm
;; Purpose: Run this file to run *all* tests written so far.

(load "test-macro.scm")

(load "../new-code/parse.scm")
(load "parse-tests.scm")
(run-parse-tests #f)