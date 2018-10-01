#!/usr/bin/guile
!#

;; This file implements the API as defined in /doc/api.


(load "../code/full-parse.scm")
(load "../code/words.scm")


(define (api-preload)
  (load-words))

(define (api-parse input)
  (full-parse input))
