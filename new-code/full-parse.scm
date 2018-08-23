#!/usr/bin/guile
!#

(load "words.scm")
(load "parse.scm")
(load "pretty-print.scm")

;; File:    full-parse.scm
;; Purpose: Turn a string representing a composite form into a
;; list of the possible expansions, in pretty-print form.


;; Step one: Split a composite form into a list of serial forms.

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


;; Step two: Expand each serial form.


;; Drop some number of jado into a predicate

(define (max-jado-tag cf)
  (cond ((not (pair? cf)) 0) ;; atoms and ()
        ((eq? (car cf) 'jado) (cadr cf))
        (#t (fold max (map max-jado-tag cf)))))

(define (jado-ify pred k)
  (if (> k (length (typelist pred)))
      (make-simple-predicate "jado-ify-failed" '(0))

      (if (= k 0)
          pred
          
          (let* ((start (1+ (max-jado-tag (gcf pred))))
                 (tags (map (lambda (x) (+ x start)) (iota k)))
                 (jado (map (lambda (x) `(jado ,x)) tags))
                 (v-do (map (lambda (x) `( do  ,x)) tags)))
            
            (cons (lambda (args)
                    `(li ,jado ,((car pred) (append v-do args))))
                  (drop (cdr pred) k))))))
  

;; Expand an XY serial-form.  Assumes a valid XY-form.
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

(define (expand-XY head tail)
  (format #t "Performing ~a XY ~a~%"
          (pred->string head)
          (pred->string tail))
  
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
  ;;(format #t "(expand *~a*)~%" cf)
  
  (cond ((is-simple-predicate cf) ;; Just one word
         cf)
        
        ((eq? 'mu (car cf)) ;; MU-form (not yet implemented)
         (make-simple-predicate "mu-not-yet-implemented" '(0)))

        ((is-RU? (car cf)) ;; RU-form (not yet implemented)
         (make-simple-predicate "RU-not-yet-implemented" '(0)))

        (#t ;; XY combination
         (let ((head (expand (car cf)))
               (tail (expand (cadr cf))))
           (if (any number? (typelist head))
               (expand-XY head tail)
               (make-simple-predicate
                "Implied-RU-not-yet-implemented" '(0)))))))
               
(define (full-parse str)
  (format #t "Performing full parse on *~a*~%" str)
  (map pred->string
       (map expand
            (get-serials (parse str)))))