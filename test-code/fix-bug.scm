#!/usr/bin/guile
!#


(load "../code/full-parse.scm")
(load "../code/words.scm")

(load-words)

(let ((ok  "mia pai")
      (fix "mia leo pai"))

  (format #t "~%Performing test...~%~%")
  (format #t "~a -> ~a~%~%" ok (full-parse ok))
  (format #t "~a -> ~a~%~%" fix (full-parse fix)))