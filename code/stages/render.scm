#!/usr/bin/guile
!#

;; File:    render.scm
;; Purpose: Turn a list of serial forms into a list of predicates

;; Input:  ( (ru (<gi> <pai>) <hui>) (ru (<gi A> <pai>) <hui>) …)
;; Output:  (  <lu to ru (lu to ru (gi) to (pai)) to (hui)>
;;             <lu to ru (gi (pai)) to (hui)>  … )


;; JADO-IFY
;; Drop some number of jado into a predicate

;; Find maximum jado ID number used in a predicate so far
(define (max-jado-tag cf)
  (cond ((not (pair? cf)) 0) ;; atoms and ()
        ((and (eq? (car cf) 'jado)
              (<= 2 (length cf)))
         (cadr cf))
        (#t (fold max (map max-jado-tag cf)))))

;; Determine whether the elements of elem can be found in ls.
;; Must be in order, but need not be consecutive.
(define (found-inside ls elem)
  (cond ((null? elem) #t)
        ((null? ls) #f)
        ((eq? (car ls) (car elem))
         (found-inside (cdr ls) (cdr elem)))
        (#t
         (found-inside (cdr ls) elem))))

;; Determine whether first k args of a pred are all top-level
;; and are in order
(define (k-well-behaved-slots pred k)
  (let ((args (take '(A B C D E F G H I J K L M
                        N O P Q R S T U V W X Y Z)
                    k)))
    (found-inside (gcf pred) args)))


(define (jado-ify pred k)
  (cond ((= k 0) pred) ;; No jado dropped

        ;; More jado than open slots; fail
        ((> k (length (typelist pred)))
         (make-fail-predicate
          (format #f
            "Jado-ify failed: could not drop ~a jado into the predicate." k)))
         
        ;; jado will all fill top-level slots, so drop
        ;; without a prenex
        ((k-well-behaved-slots pred k)
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
      (make-fail-predicate "Could not mu-ify the predicate.")

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
  (cond ((eq? 'mu (car cf)) ;; MU-form
         (mu-ify (expand (cadr cf))))
        
        ((is-RU? (car cf)) ;; RU-form
         (ru-ify (expand (cadr cf))
                 (expand (caddr cf))
                 (car cf)))
        
        ((eq? 'xy (car cf)) ;; XY-form
         (let ((head (expand (cadr cf)))
               (tail (expand (caddr cf))))
           
           (if (any number? (typelist head))
               (expand-XY head tail)       ;; XY case
               (ru-ify head tail 'ru))))   ;; Implicit-ru case

        (#t cf))) ;; Just one word

;; Perform the "render" stage on a list of serial forms

(define (stage-render interpret-output)
  (map expand interpret-output))