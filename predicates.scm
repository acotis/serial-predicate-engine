
(use-modules (srfi srfi-1))

;; Symbols
;; ----------------
;; c = concrete arg
;; 0 = proposition
;; 1 = property
;; 2 = relation
;; 3 = ...
;; jado = lambda


;; Helper functions
;;   (fold)       Fold a function like: (f (f (A B)) C)
;;   (fold-right) Fold a function like: (f A (f (B C)))
;;   (find-first) Return index of first e where (fun e) is true.

(define (fold fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fold fun (cons (fun (car ls) (cadr ls)) (cddr ls)))))

(define (fold-right fun ls)
  (if (null? (cdr ls))
      (car ls)
      (fun (car ls) (fold-right fun (cdr ls)))))

(define (find-first fun ls)
  (if (fun (car ls))
      0
      (+ 1 (find-first fun (cdr ls)))))


;; Create a simple predicate.
;; (make-simple-predicate "leo" '(c 1)) ->
;; '( ((A B)->(leo A B)) c 1 )

(define (make-simple-predicate name typelist)
  (cons (lambda (args)
          (cons name args))
        typelist))


;; Get the canonic form or typelist of a predicate.
;; Example: (gcf leo)      = '(leo A B)
;; Example: (typelist leo) = '(c 1)

(define (gcf pred)
  ((car pred)
   (take '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
         (length (cdr pred)))))

(define (typelist pred)
  (cdr pred))


;; Take an n-ary predicate and return an (n-k)-ary predicate
;; where the first k slots are all jado
;; Example (gcf (jado-ify leo 1)) = '(leo jado A)

(define (max-jado-tag cf)
  (cond ((not (pair? cf)) 0) ;; atoms and ()
        ((eq? (car cf) 'jado) (cadr cf))
        (#t (fold max (map max-jado-tag cf)))))

(define (jado-ify pred k)
  (if (= k 0)
      pred
      
      (let* ((start (1+ (max-jado-tag (gcf pred))))
             (tags (map (lambda (x) (+ x start)) (iota k)))
             (jado (map (lambda (x) `(jado ,x)) tags))
             (v-do (map (lambda (x) `( do  ,x)) tags)))
            
        (cons (lambda (args)
                `(li ,jado ,((car pred) (append v-do args))))
              (drop (cdr pred) k)))))

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