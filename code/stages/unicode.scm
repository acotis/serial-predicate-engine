#!/usr/bin/guile
!#


(set-port-encoding! (current-output-port) "UTF-8")

(format #t "Hello00~%")
(format #t "~a~%" (integer->char #x0100))
(format #t "~a~%" (char->integer (integer->char #x1EAF)))
(format #t "~a~%" (char->integer #\?))

(format #t "a~a~%" #\◌̉ )

(format #t "ả~%")