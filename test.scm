#!/usr/bin/guile
!#

(load "serial_expansion.scm")
(use-syntax (ice-9 syncase))
(use-modules (ice-9 pretty-print))


(define-syntax test
  (syntax-rules ()
    
    ((test call expected display-anyway)
     (let ((result call))  
       (if (equal? expected result)
           (begin (if display-anyway
                      (begin (format #t "Test:     ~a~%" 'call)
                             (format #t "Result:   ~a~%~%" result)))
                  #t)
           
           (begin (format #t "Test:     ~a~%" 'call)
                  (format #t "Expected: ~a~%" expected)
                  (format #t "Result:   ~a~%~%" result)
                  #f))))

    ((test call expected)
     (test call expected #f))))


(if (and

     ;; fill-slots helper functions
     (test (has-open-slot 'c) #t #t)
     (test (has-open-slot 3) #t #t)
     (test (has-open-slot 'suq) #f #t)
     (test (has-open-slot '(dua c 0)) #t #t)
     (test (has-open-slot '(dua jado (mai ji c))) #t #t)
     (test (has-open-slot '(dua jado (mai ji suq))) #f #t)

     ;; fill-slots function
     (test (fill-one-slot '(mai c c) 'ji)
           '(mai ji c)
           #t)

     )

    (format #t "All tests passed.~%")
    (format #t "One or more tests failed!~%"))

