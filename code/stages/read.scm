#!/usr/bin/guile
!#

;; File:    read.scm
;; Purpose: Turn a raw input string into a list of cmavo & roots

;; Input:  "to tu gi pai to hui"
;; Output: (to ru "gi" "pai" to "hui")

(load "../utilities.scm")


;; Split the input into space-separated words

(define (split-on-spaces string)
  (remove (lambda (s) (equal? s ""))
          (string-split string #\Space)))

;; Replace cmavo strings with their symbol counterparts

(define (replace-cmavo-strings com)
  (map (lambda (s)
         (let ((sym (string->symbol s)))
           (if (member sym cmavo) sym s)))
       com))


;; Perform the "read" stage on an input string.

(define (stage-read input)
  (replace-cmavo-strings
   (split-on-spaces
    input)))