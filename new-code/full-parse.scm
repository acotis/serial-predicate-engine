#!/usr/bin/guile-1.8
!#


(load "stages/read.scm")
(load "stages/parse.scm")
(load "stages/interpret.scm")
(load "stages/render.scm")
(load "stages/pretty-print.scm")


(define (full-parse input)
  (stage-pretty-print
   (stage-render
    (stage-interpret
     (stage-parse
      (stage-read
       input))))))