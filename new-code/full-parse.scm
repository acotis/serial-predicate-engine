#!/usr/bin/guile
!#

(load "words.scm")
(load "parse.scm")
(load "pretty-print.scm")

;; File:    full-parse.scm
;; Purpose: Turn a string representing a composite form into a
;; list of the possible expansions, in pretty-print form.


;; Split a composite form into a list of serial forms.

(define (get-serials cf)
  (cond ((string? cf) ;; Single word
         (make-word cf))
        
        ;; MU-form
        ((eq? 'mu (car cf))
         (map (lambda (f) (list 'mu f))
              (get-serials (cadr cf))))

        ;; XY-form
        ((= 2 (length cf))
         (let ((head (get-serials (car cf)))
               (tail (get-serials (cadr cf))))
           (fold append
                 (map (lambda (h)
                        (map (lambda (t)
                               (list h t))
                             tail))
                      head))))

        ;; RU-form
        (#t
         (map (lambda (f)
                (cons (car cf) f))
              (get-serials (cdr cf))))))


;; JADO-IFY
;; Drop some number of jado into a predicate

;; Find maximum jado ID number used in a predicate so far
(define (max-jado-tag cf)
  (cond ((not (pair? cf)) 0) ;; atoms and ()
        ((eq? (car cf) 'jado) (cadr cf))
        (#t (fold max (map max-jado-tag cf)))))

;; Determine whether first k args of a pred are all top-level
(define (top-level-slots pred k)
  (let* ((canaries (map (lambda (n) (gensym)) (iota k)))
         (plugged ((predicate pred)
                   (append canaries (make-list 100 'foo)))))
    (every (lambda (n) (member n plugged))
           canaries)))


(define (jado-ify pred k)
  (cond ((= k 0) pred) ;; No jado dropped

        ;; More jado than open slots; fail
        ((> k (length (typelist pred)))
         (make-fail-predicate
          (format #f
            "Jado-ify failed: could not drop ~a jado into the predicate." k)))
         
        ;; jado will all fill top-level slots, so drop
        ;; without a prenex
        ((top-level-slots pred k)
         (cons (lambda (args)
                 ((predicate pred)
                  (append (make-list k '(jado)) args)))
               (drop (typelist pred) k)))

        ;; Some jado will fill non-top slot, prenex needed
        (#t
         (let* ((start (1+ (max-jado-tag (gcf pred))))
                (tags (map (lambda (x) (+ x start)) (iota k)))
                (jado (map (lambda (x) `(jado ,x)) tags))
                (v-do (map (lambda (x) `( do  ,x)) tags)))
           
           (cons (lambda (args)
                   `(li ,jado ,((predicate pred)
                                (append v-do args))))
                 (drop (typelist pred) k))))))


;; MU-IFY

(define (swap-first-two ls)
  (cons (cadr ls) (cons (car ls) (cddr ls))))

(define (mu-ify pred)
  (if (< (length (typelist pred)) 2)
      (make-simple-predicate "mu-ify-failed" '(0))

      (cons (lambda (args)
              ((predicate pred)
               (swap-first-two args)))
            (swap-first-two (typelist pred)))))


;; RU-IFY

(define (zip-typelists atypes btypes)
  (cond ((null? atypes) btypes)
        ((null? btypes) atypes)
        (#t (cons (if (eq? (car atypes) (car btypes))
                      (car atypes)
                      '?)
                  (zip-typelists (cdr atypes) (cdr btypes))))))

(define (ru-ify apred bpred ru)
  (cons (lambda (args)
          `("lu" to ,ru ,((predicate apred) args)
            to ,((predicate bpred) args)))
        (zip-typelists (typelist apred) (typelist bpred))))


;; Expand an XY serial-form.  Assumes a valid XY-form.
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

(define (expand-XY head tail)
  ;;(format #t "Performing ~a XY ~a~%"
  ;;        (pred->string head)
  ;;        (pred->string tail))
  
  (let* ((hpred (predicate head))
         (htypes (typelist head))
         
         (slot ((lambda (k)
                  (if (= k (- (length htypes) 1)) 0 (+ k 1)))
                (find-first number?
                            (append (cdr htypes)
                                    (list (car htypes))))))
         
         (arity (list-ref htypes slot))
         (new-tail (jado-ify tail arity))

         (tpred (predicate new-tail))
         (ttypes (typelist new-tail))
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


;; Full function

(define (expand cf)
  (cond ((is-simple-predicate cf) ;; Just one word
         cf)
        
        ((eq? 'mu (car cf)) ;; MU-form (not yet implemented)
         (mu-ify (expand (cadr cf))))

        ((is-RU? (car cf)) ;; RU-form (not yet implemented)
         (ru-ify (expand (cadr cf))
                 (expand (caddr cf))
                 (car cf)))
         
        (#t ;; XY combination
         (let ((head (expand (car cf)))
               (tail (expand (cadr cf))))
           (if (any number? (typelist head))
               (expand-XY head tail)       ;; XY case
               (ru-ify head tail 'ru)))))) ;; Implicit-ru case

;; Unmemoized full-parse function

(define (full-parse-unmemoized str)
  (format #t "Performing full parse on *~a*~%" str)
  (map pred->string
       (map expand
            (get-serials (parse str)))))

;; Memoization of full-parse function

(define full-parse-lookup-table '())

(define (full-parse str)
  (let ((lookup (assoc str full-parse-lookup-table)))
    (if lookup
        (cdr lookup)
        
        (let ((result (full-parse-unmemoized str)))
          (set! full-parse-lookup-table
                (cons (cons str result)
                      full-parse-lookup-table))
          result))))