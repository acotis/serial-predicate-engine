
(load "predicates.scm")
(load "utilities.scm")


;; It's really stupid that I can't do this with let-over-define.
;; Oh well.

;; Signature getting and setting

(define signatures (make-hash-table))
  
(define (add-word word typelist)
  (hash-set! signatures word typelist))

(define (get-signature word)
  (or (hash-ref signatures word)
      '()))


;; Make a word given its name (looks up its signature in the
;; hash table)

(define (make-word word)
  (map (lambda (typelist) (make-simple-predicate word typelist))
       (get-signature word)))


;; Remove from a word all simple predicates whose typelists are
;; prefixes to typelists of other predicates of that word.
;; (i.e., remove all non-SPA's)
;; ----- CURRENTLY NOT IN USE.
(define (keep-SPA word)
  (reverse
   (fold (lambda (preds next-pred)
           (if (starts-with? (typelist (car preds))
                             (typelist next-pred))
               (cons next-pred (cdr preds))
               (cons next-pred preds)))
         ((lambda (k) (cons (list (car k)) (cdr k)))
          (sort word
                (lambda (a b) (< (length (typelist a))
                                 (length (typelist b)))))))))

;; Basic word tools

;; Convert all the predicates of a word to canonic format
(define (wgcf word)
  (map gcf word))

(define (wgtf word)
  (map gtf word))

(define (wgff word)
  (map gff word))


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

