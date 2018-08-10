
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
;; Examples:
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

(define (expand-binary head tail)
  (let ((mix (lambda (ls)
               (append (cddr ls)
                       (list (cadr ls)))))
        (fix (lambda (ls)
               (append (list (car head))
                       (take-right ls 1)
                       (drop-right ls 1)))))
    (fix
     (replace-first (mix head)
                    number?
                    (lambda (s)
                      (fill-slots tail (make-list s 'jado)))))))


;; Expand poly-predicates
;; Example:
;; ((dua c 0) (leo c 1) (mai c c)) = (dua c (leo c (mai jado c)))

(define (expand predicates)
  (fold-right expand-binary predicates))