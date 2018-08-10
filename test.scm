#!/usr/bin/guile
!#

(load "serial_expansion.scm")
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


(let ((show-gcf #t)  ;; Show (gcf) "get canonic form" tests
      (show-jdf #t)  ;; Show (jado-ify) tests

      (mai-2 (make-simple-predicate "mai" '(c c)))
      (dua-2 (make-simple-predicate "dua" '(c 0)))
      (soq-3 (make-simple-predicate "soq" '(1 c 1)))
      (jai-0 (make-simple-predicate "jai" '())))
      
  (if (and
       
       ;; gcf
       (test (gcf mai-2) '("mai" A B) show-gcf)
       (test (gcf dua-2) '("dua" A B) show-gcf)
       (test (gcf soq-3) '("soq" A B C) show-gcf)
       (test (gcf jai-0) '("jai") show-gcf)

       ;; jado-ify
       (test (gcf (jado-ify mai-2 1))
             '("mai" jado A)
             show-jdf)
       (test (gcf (jado-ify dua-2 2))
             '("dua" jado jado)
             show-jdf)
       (test (gcf (jado-ify soq-3 1))
             '("soq" jado A B)
             show-jdf)
       (test (gcf (jado-ify soq-3 0))
             '("soq" A B C)
             show-jdf)
       (test (cdr (jado-ify soq-3 1))
             '(c 1)
             show-jdf)
       
       )
      
      (format #t "All tests passed.~%")
      (format #t "One or more tests failed!~%")))