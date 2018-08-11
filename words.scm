
(load "predicates.scm")
(load "utilities.scm")


;; It's really stupid that I can't do this with let-over-define.
;; Oh well.


;; Signature getting and setting

(define signatures (make-hash-table))
  
(define (set-signature word typelist)
  (hash-set! signatures word typelist))

(define (get-signature word)
  (or (hash-ref signatures word)
      '()))


;; Build-word and helpers

(define (remove-prefixes sorted-sig)
  (if (<= (length sorted-sig) 1)
      sorted-sig
      (let ((first (car sorted-sig))
            (second (cadr sorted-sig))
            (rest (remove-prefixes (cdr sorted-sig))))
        (if (starts-with? first second)
            rest
            (cons first rest)))))

(define (stopping-points signature)
  (remove-prefixes
   (sort signature (lambda (a b) (< (length a) (length b))))))
   
(define (build-word word)
  (map (lambda (typelist) (cons word typelist))
       (stopping-points (get-signature word))))


;; compose-binary

(define (compose-binary head tail)
  (fold append
        (map (lambda (head-pred)
               (map (lambda (tail-pred)
                      (expand-binary head-pred tail-pred))
                    tail))
             head)))


;; compose (for word structures)
;; compose-words (for words passed as "mai" or "jeo")

