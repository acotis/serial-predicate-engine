#!/usr/bin/guile
!#

;; This file implements the API as defined in /doc/api.


(load "../code/full-parse.scm")
(load "../code/words.scm")


(define api-debug #f)


;; Input: None
;; Output: None
;; Side effects: Prepare the program to parse strings

(define (api-preload)
  (if api-debug (format #t "API call: preload~%"))
  (load-words)

  ;; Necessary to print unicode characters properly
  ;; Note: This procedure of setting the port encoding correctly
  ;;       seems a bit delicate.  Be careful.
  (set-port-encoding! (current-output-port) "UTF-8")
  (fluid-set! %default-port-encoding "utf-8"));;Taken from online


;; Input: A string representing a serial predicate to be parsed
;; Output: A list of strings representing the possible expansions

(define (api-parse input)
  (if api-debug (format #t "API call: parse \"~a\"~%" input))
  (full-parse input))