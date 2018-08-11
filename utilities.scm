

;; Fold a function like: (f (f (A B)) C)
;; (Differs from standard definition by not requiring a seed)

(define (fold fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fold fun (cons (fun (car ls) (cadr ls)) (cddr ls)))))

;; Fold a function like (f A (f B C))
;; (Differs from standard definition by not requiring a seed)

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold-right fun (cdr ls)))))

;; Return index of the first element e of ls such that (fun e)
;; returns true.  Assumes such an element exists.

(define (find-first fun ls)
  (if (fun (car ls))
      0
      (+ 1 (find-first fun (cdr ls)))))

;; Determine whether the list b begins with the list a.
;; For example, '(1 2 3 4) begins with '(1 2) and '()
;;                         but not '(2 3 4) or '(1 2 5).

(define (starts-with? a b)
  (or (null? a)
      (and (equal? (car a) (car b))
           (starts-with? (cdr a) (cdr b)))))
