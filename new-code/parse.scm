#!/usr/bin/guile
!#

;; File:    parse.scm
;; Purpose: Translate a string representing a serial predicate
;;          into a parsed-composite structure (see doc file
;;          "new-structure.odt" for more info).

(load "utilities.scm")
(use-modules (srfi srfi-1))


;; Members of RU

(define RU '(ru ra ro ri re roi))
(define cmavo (append RU '(to mu)))


;; Split the input string into a list of word strings

(define (split-clean string)
  (remove (lambda (s) (equal? s ""))
          (string-split string #\Space)))


;; Replace strings "mu", "to", "ru", "ra", etc with symbols

(define (replace-cmavo-strings com)
  (map (lambda (s)
         (let ((sym (string->symbol s)))
           (if (member sym cmavo) sym s)))
       com))


;; Input: ( A to ru B to C )
;; Output: ( A (to ru (B) (C)) )

;; Split a composite just before the first "to"
(define (split-first-to com)
  (call-with-values
      (lambda () (span (lambda (s) (not (eq? s 'to))) com))
    list))

;; Find the tail of a toru statement, given the current toru
;; nesting depth.  Return number of words AFTER pthe final to.
(define (find-toru-tail com depth)
  (if (= depth 0)
      (length com)
      (let ((next-to (cadr (split-first-to com))))
        (if (member (cadr next-to) RU)
            (find-toru-tail (cddr next-to) (+ depth 1))
            (find-toru-tail (cdr next-to) (- depth 1))))))

;; Split (anything TO RU a TO b) into (anything (to ru a b)).
;; Assumes that there is a toru construct in the composite.
(define (fork-at-toru com)
  (let* ((split (split-first-to com))
         (after (cadr split)) ;; after and including toru
         (under (cddr after)) ;; after, not including toru
         (close-index (find-toru-tail under 1)))

    (list (car split)
          (list 'to
                (cadr after)
                (drop-right under (+ close-index 1))   
                (drop under
                      (- (length under) close-index))))))


;; Parse a composite into a parse-form
;; i.e. ("jai") -> "jai"
;;      (to ru "kuai" to "tua") -> (ru "kuai" "tua")

(define (parse-composite com)
  (format #t "(parse-composite ~a)~%" com)
  
  (cond ((= (length com) 1)
         (car com))

        ((every string? com)
         (fold-right list com))
         
        ;; ((member 'to com)
        ;;  (let* ((fork (fork-at-toru com))
        ;;         (toru (cadr fork)))
           
        ;;    ;;(format #t "fork = ~a~%" fork)
        ;;    ;;(format #t "toru = ~a~%" toru)
           
        ;;    (append (car fork)
        ;;            (list
        ;;             (list (cadr toru)
        ;;                   (parse-composite (caddr toru))
        ;;                   (parse-composite (cadddr toru)))))))
        
        (#t '())))


;; Dummy function
(define (parse string)
  (parse-composite
   (replace-cmavo-strings
    (split-clean string))))