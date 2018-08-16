#!/usr/bin/guile
!#

;; File: test.scm.
;; Defines the (test) and (run-tests) macros for use in
;; other test files.

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
(define-syntax count-fails
  (syntax-rules ()
    ((and-all exp ...)
     (let ((pass 0)
           (fail 0))
       (begin (if exp
                  (set! pass (+ pass 1))
                  (set! fail (+ fail 1)))
              ...
              (cons fail pass))))))


;; Run a series of tests.  Keep going even if a test fails.
;; If any tests fail, call on-failure with (fail . pass) as arg.
;; If all tests pass, call on-success with (fail . pass) as arg.
(define-syntax run-tests
  (syntax-rules ()
    ((run-tests ((call expected) ...)
                display-anyway
                on-failure
                on-success)

     (let ((result (count-fails
                    (test call expected display-anyway)
                    ...)))
       (if (= (car result) 0)
           (on-success result)
           (on-failure result))))

    ((run-tests ((call expected) ...)
                display-anyway
                on-failure)
     (run-tests ((call expected) ...)
                display-anyway
                on-failure
                '()))))