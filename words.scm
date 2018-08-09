
(load "serial_expansion.scm")


;; It's really stupid that I can't do this with let-over-define.
;; Oh well.
(define signatures (make-hash-table))
  
(define (set-signature word typelist)
  (hash-set! signatures word typelist))

(define (get-signature word)
  (or (hash-ref signatures word)
      '()))

(define (build-word word)
  (map (lambda (typelist) (cons word typelist))
       (get-signature word)))