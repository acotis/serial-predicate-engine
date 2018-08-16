#!/usr/bin/guile
!#


;; Fold a function like (f A (f B C))
;; (Differs from standard definition by not requiring a seed)

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold-right fun (cdr ls)))))
