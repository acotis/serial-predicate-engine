#!/usr/bin/guile
!#

;; This file implements the API as defined in /doc/api.


(load "../code/full-parse.scm")
(load "../code/words.scm")


(define api-debug #f)

(define (api-preload)
  (if api-debug (format #t "API call: preload~%"))
  (load-words))

(define (api-parse input)
  (if api-debug (format #t "API call: parse \"~a\"~%" input))
  (full-parse input))
