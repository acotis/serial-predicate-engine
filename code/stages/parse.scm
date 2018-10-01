#!/usr/bin/guile
!#

;; File:    parse.scm
;; Purpose: Turn a list of cmavo & roots into a tree-structure
;;          representing a composite.

;; Input:  (to ru "gi" "pai" to "hui")
;; Output: (ru ("gi" "pai") "hui")


(load "../utilities.scm")
(use-modules (srfi srfi-1))


;; RU
;; Replace last instance of ...p1 RU p2... with ...(RU p1 p2)...

(define (fold-last-ru com)
  (cond ((> (count is-RU? com) 1)
         (cons (car com) (fold-last-ru (cdr com))))

        ((not (is-RU? (cadr com)))
         (cons (car com) (fold-last-ru (cdr com))))

        (#t
         (cons (list (cadr com) (car com) (caddr com))
               (cdddr com)))))


;; MU
;; Fold all instances of ...MU p1... into ...(MU p1)...
;; Note that this only replaces innermost instances of MU,
;; so you still may need to call this function more than once

(define (fold-inner-mu com)
  (if (not (member 'mu com))
      com
      
      (if (and (eq? 'mu (car com))
               (not (eq? 'mu (cadr com))))
          (cons (list (car com) (cadr com))
                (fold-inner-mu (cddr com)))
          (cons (car com)
                (fold-inner-mu (cdr com))))))


;; TO RU
;; Split the highest-level (A to ru B to C) into
;; ((A) (to ru (B) (C))).  

(define (before-first-to com)
  (if (eq? 'to (car com))
      '()
      (cons (car com) (before-first-to (cdr com)))))

(define (find-top-level com level)
  (if (= 0 level)
      com
      (find-top-level (cdr com)
                      (if (eq? 'to (car com))
                          (if (is-RU? (cadr com))
                              (+ level 1)
                              (- level 1))
                          level))))

(define (fork-toru com)
  (let* ((before (before-first-to com))
         (after (drop com (length before)))
         (under (drop after 2))
         (branch-b (find-top-level under 1))
         (branch-a (drop-right under (+ 1 (length branch-b)))))
    (list before
          (list 'to (cadr after) branch-a branch-b))))


;; Parse a composite into a parse-form
;; i.e. ("jai") -> "jai"
;;      (to ru "kuai" to "tua") -> (ru "kuai" "tua")

(define (parse-composite com)
  (cond (
         ;; Parse outermost TO RU ... TO ... construction
         (member 'to com)
         (let* ((fork (fork-toru com))
                (before (car fork))
                (toru (cadr fork)))
           
           (parse-composite
            (append before
                    (list
                     (list (cadr toru)
                           (parse-composite (caddr toru))
                           (parse-composite (cadddr toru))))))))
        
        ;; Fold innermost MU particles
        ((member 'mu com)
         (parse-composite (fold-inner-mu com)))
        
        ;; Fold last RU pairing
        ((any is-RU? com)
         (parse-composite (fold-last-ru com)))
        
        ;; If only one parse-form, return unchanged
        ((= 1 (length com))
         (car com))
        
        ;; If multiple parse-forms, serialize
        (#t
         (list (car com)
               (parse-composite (cdr com))))))


;; Perform the "parse" stage on the output of the "read" stage.

(define (stage-parse read-output)
  (parse-composite read-output))