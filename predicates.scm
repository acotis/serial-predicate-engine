
(load "serial_expansion.scm")


;; It's really stupid that I can't do this with let-over-define.
;; Oh well.
(define signatures (make-hash-table))
  
(define (set-signature predicate typelist)
  (hash-set! signatures predicate typelist))

(define (get-signature predicate)
  (or (hash-ref signatures predicate)
      '()))