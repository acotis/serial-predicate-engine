
;; Symbols
;; ----------------
;; c = concrete arg
;; 0 = proposition
;; 1 = property
;; 2 = relation
;; 3 = ...
;; jado = lambda


;; Replace the first elem e of ls such that (test e) is true
;; with (transform e)
(define (replace-first ls test transform)
  (if (test (car ls))
      (cons (transform (car ls)) (cdr ls))
      (cons (car ls) (replace-first (cdr ls) test transform))))

;; Defining my own fold functions to get the obviously correct
;; no-seed-needed implementation.
(define (fold fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fold fun (cons (fun (car ls) (cadr ls)) (cddr ls)))))

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold fun (cdr ls)))))


;; Formatting

(define (is-slot-marker e)
  (or (eq? e 'c) (number? e)))

(define (is-filled-slot e)
  (and (not (is-slot-marker e))
       (not (list? e))))
