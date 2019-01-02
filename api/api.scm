#!/usr/bin/guile
!#

;; This file implements the API as defined in /doc/api.


(load "../code/full-parse.scm")
(load "../code/words.scm")


(define api-debug #t)


;; Input: None
;; Output: None
;; Side effects: Prepare the program to parse strings

(define (api-preload)
  (if api-debug (format #t "API call: preload~%"))
  (load-words))


;; Input: A string representing a serial predicate to be parsed
;; Output: A list of strings representing the possible expansions

(define (api-parse input)
  (if api-debug (format #t "API call: parse \"~a\"~%" input))
  (let ((result (full-parse input)))
    (if api-debug (format #t "  [Result: ~a]~%" result))
    result))