#!/usr/bin/guile
!#


;; Fold a function like (f A (f B C))
;; (Differs from standard definition by not requiring a seed)

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold-right fun (cdr ls)))))


;; Count how many elements of ls satisfy fun

(define (count fun ls)
  (if (null? ls)
      0
      (+ (if (fun (car ls)) 1 0)
         (count fun (cdr ls)))))


;; Find index of first elem e of ls which satisfies fun

(define (find-first fun ls)
  (if (fun (car ls))
      0
      (+ 1 (find-first fun (cdr ls)))))