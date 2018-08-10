
;(require-extension sequences)
(use-modules (srfi srfi-1))

;; Symbols
;; ----------------
;; c = concrete arg
;; 0 = proposition
;; 1 = property
;; 2 = relation
;; 3 = ...
;; jado = lambda


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


;; Return the index of first e in ls such that (fun e) is true
;; (Assumes that such an e exists)
(define (find-first fun ls)
  (if (fun (car ls))
      0
      (+ 1 (find-first fun (cdr ls)))))


;; Create a simple predicate given its name and typelist
;; (The predicate function assumes that the correct number
;;  of args are passed to it)

(define (make-simple-predicate name typelist)
  (cons (lambda (args)
          (cons name args))
        typelist))


;; Get the canonic form of a predicate.
;; Examples: (leo A B) or (dua A (mai B C))

(define (gcf pred)
  ((car pred)
   (take '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
         (length (cdr pred)))))


;; Take an n-ary predicate and return an (n-k)-ary predicate
;; where the first k slots are all jado

(define (jado-ify pred k)
  (cons (lambda (args)
          ((car pred) (append (make-list k 'jado) args)))
        (drop (cdr pred) k)))
  

;; Expand a two-part predicate into one predicate.
;; Assumes head has an abstraction place.
;; Examples:
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

(define (expand-binary head tail)
  (let* ((hpred (car head))
         (htypes (cdr head))
         
         (slot ((lambda (k)
                  (if (= k (- (length htypes) 1)) 0 (+ k 1)))
                (find-first number?
                            (append (cdr htypes)
                                    (list (car htypes))))))
         
         (arity (list-ref htypes slot))
         (new-tail (jado-ify tail arity))

         (tpred (car new-tail))
         (ttypes (cdr new-tail))
         (tcount (length ttypes))

         (etypes (append (take htypes slot)
                         ttypes
                         (drop htypes (+ slot 1)))))    
    (cons (lambda (args)
            (hpred (append
                    (take args slot)
                    (list (tpred (take (drop args slot) tcount)))
                    (drop args (+ slot tcount)))))
          etypes)))


;; Expand poly-predicates
;; Example:
;; ((dua c 0) (leo c 1) (mai c c)) = (dua c (leo c (mai jado c)))

(define (expand predicates)
  (fold-right expand-binary predicates))