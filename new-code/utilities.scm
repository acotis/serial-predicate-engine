#!/usr/bin/guile
!#


;; Fold a function like (f A (f B C))
;; (Differs from standard definition by not requiring a seed)

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold-right fun (cdr ls)))))


;; Fold a function like (f (f A B) C)
;; (See above note about difference from standard definition)

(define (fold fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fold fun (cons (fun (car ls) (cadr ls)) (cddr ls)))))


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


;; Replaces the first e in ls such that (test e) is true
;; with (transform e)

(define (replace-first ls test transform)
  (if (test (car ls))
      (cons (transform (car ls)) (cdr ls))
      (cons (car ls) (replace-first (cdr ls) test transform))))


;; Members of RU

(define RU '(ru ra ro ri re roi))
(define cmavo (append RU '(to mu)))

(define (is-RU? e)
  (member e RU))

(define (is-cmavo? e)
  (member e cmavo))