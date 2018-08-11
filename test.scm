#!/usr/bin/guile
!#

(load "predicates.scm")
(load "words.scm")
(use-syntax (ice-9 syncase))


(define-syntax test
  (syntax-rules ()
    
    ((test call expected display-anyway)
     (let ((result call))  
       (if (equal? expected result)
           (begin (if display-anyway
                      (begin
                        (format #t "Test:     ~a~%" 'call)
                        (format #t "Result:   ~a~%~%" result)))
                  #t)
           
           (begin (format #t "Test:     ~a~%" 'call)
                  (format #t "Expected: ~a~%" expected)
                  (format #t "Result:   ~a~%~%" result)
                  #f))))

    ((test call expected)
     (test call expected #f))))


;; (and) but with no short-circuiting
(define-syntax and-all
  (syntax-rules ()
    ((and-all exp ...)
     (let ((all #t))
       (begin (if (not exp) (set! all #f))
              ...
              all)))))


;; Run a series of tests.  Run all tests even after one fails.
(define-syntax run-tests
  (syntax-rules ()
    ((run-tests ((call expected) ...)
                display-anyway
                on-failure
                on-success)

     (if (and-all (test call expected display-anyway)
                  ...)
         on-success
         on-failure))

    ((run-tests ((call expected) ...)
                display-anyway
                on-failure)
     (run-tests ((call expected) ...)
                display-anyway
                on-failure
                '()))))


;; Which tests to display even on success

(define show-gcf #f)
(define show-jado-ify #t)
(define show-expand-binary #t)
(define show-expand #t)

;; Some sample predicates

(define jai-0 (make-simple-predicate "jai" '()))
(define jai-1 (make-simple-predicate "jai" '(c)))
(define gi-1 (make-simple-predicate "gi" '(0)))
(define mai-2 (make-simple-predicate "mai" '(c c)))

(define dua-2 (make-simple-predicate "dua" '(c 0)))
(define tua-2 (make-simple-predicate "tua" '(c 0)))

(define kuai-2 (make-simple-predicate "kuai" '(c 1)))
(define leo-2 (make-simple-predicate "leo" '(c 1)))
(define jeaq-2 (make-simple-predicate "jeaq" '(c 1)))
      
(define soq-3 (make-simple-predicate "soq" '(c 1 c)))


;; (gcf) "Get canonical form" tests

(run-tests
 ( ((gcf mai-2) '("mai" A B))
   ((gcf dua-2) '("dua" A B))
   ((gcf soq-3) '("soq" A B C))
   ((gcf jai-0) '("jai")) )

 show-gcf
 (begin (format #t "One or more (gcf) tests failed.~%")
        (quit)))


;; (jado-ify) tests

(let ((mai-2/1 (jado-ify mai-2 1))
      (dua-2/2 (jado-ify dua-2 2))
      (soq-3/1 (jado-ify soq-3 1))
      (soq-3/0 (jado-ify soq-3 0)))

  (run-tests
   ( ((gcf mai-2/1)      '(li ((jado 1)) ("mai" ( do 1) A)))
     ((typelist mai-2/1) '(c))
     
     ((gcf dua-2/2)      '(li ((jado 1) (jado 2))
                              ("dua" ( do 1) ( do 2))))
     ((typelist dua-2/2) '())
     
     ((gcf soq-3/1)      '(li ((jado 1)) ("soq" ( do 1) A B)))
     ((typelist soq-3/1) '(1 c))
     
     ((gcf soq-3/0)      '("soq" A B C))
     ((typelist soq-3/0) '(c 1 c)) )

   show-jado-ify
   (begin (format #t "One or more (jado-ify tests) failed.~%")
          (quit))))
        
;; expand-binary and expand
(let ((dua-mai (expand-binary dua-2 mai-2))
      (leo-mai (expand-binary leo-2 mai-2))
      (soq-dua (expand-binary soq-3 dua-2))
      (soq-gi  (expand-binary soq-3 gi-1))
      (dua-jai (expand-binary dua-2 jai-0))
      (ktjj (expand (list kuai-2 tua-2 jeaq-2 jai-1))))
  
  (run-tests 
   ( ((gcf dua-mai) '("dua" A ("mai" B C)))
     ((typelist dua-mai) '(c c c))
          
     ((gcf leo-mai) '("leo" A (li ((jado 1)) ("mai" ( do 1) B))))
     ((typelist leo-mai) '(c c))
          
     ((gcf soq-dua) '("soq" A
                      (li ((jado 1)) ("dua" ( do 1) B))
                      C))
     ((typelist soq-dua) '(c 0 c))

     ((gcf soq-gi) '("soq" A (li ((jado 1)) ("gi" ( do 1))) B))
     ((typelist soq-gi)'(c c))
     
     ((gcf dua-jai) '("dua" A ("jai")))
     ((typelist dua-jai) '(c)) )

   show-expand-binary
   (begin (format #t
                  "One or more (expand-binary) tests failed.~%")
          (quit)))

  (run-tests
   ( ((gcf ktjj)
      '("kuai" A
        (li ((jado 2))
            ("tua" ( do 2)
             ("jeaq" B
              (li ((jado 1)) ("jai" ( do 1))))))))
     ((typelist ktjj) '(c c)) )

   show-expand
   (begin (format #t "One or more (expand) tests failed.~%")
          (quit))))

(format #t "All tests passed.~%")