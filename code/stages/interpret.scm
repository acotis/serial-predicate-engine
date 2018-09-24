#!/usr/bin/guile
!#

;; File:    interpret.scm
;; Purpose: Turn a composite form into a list of serial forms,
;;          filtering out any undesired forms.

;; Input:  (ru (“gi” “pai”) “hui”)
;; Output: ( (ru (<gi> <pai>) <hui>) (ru (<gi A> <pai>) <hui>) …)


(load "../words.scm")


;; Split a composite form into a list of serial forms, using all
;; combinations of arities.

(define (get-serials cf)
  ;;(format #t "(get-serials ~a)~%" cf)
  
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


;; Filter out all but the last (all-full-arities) interpretation

(define (filter-serials serials)
  (list (last serials)))


;; Perform the "interpret" stage on a parsed composite form.

(define (stage-interpret parse-output)
  (filter-serials (get-serials parse-output)))