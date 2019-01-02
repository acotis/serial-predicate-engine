#!/usr/bin/guile
!#

;; File: test.scm.
;; Defines the (test) and (run-tests) macros for use in
;; other test files.

(use-syntax (ice-9 syncase))


;; Run a single test.  On fail, print out the call, the expected
;; result, and the actual result.  On success do nothing, unless
;; display-anyway is #t (default #f), in which case print out
;; the call the the actual result.
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
                  (set-port-encoding! (current-output-port) "ISO-8859-1")
                  (format #t "Expected: ~a~%" expected)
                  (set-port-encoding! (current-output-port) "UTF-8")
                  (format #t "Result:   ~a~%~%" result)
                  #f))))

    ((test call expected)
     (test call expected #f))))


;; Evaluate a series of expressions, and return a pair containing
;; the number that resolved to #f cons'd to the number that
;; resolved to #t.
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


;; Utility function: print out the message "x/y __ tests failed."
;; where x, y, and ___ are supplied as arguments
(define (print-fail-report counts name)
  (format #t (string-append "~a/~a " name " tests failed.~%")
          (car counts) (+ (car counts) (cdr counts))))

;; Utility function: return a function of one variable that
;; passes that variable and a specified string to
;; print-fail-report, then returns false
(define (fail-function name)
  (lambda (r)
    (begin (print-fail-report r name)
           #f)))

;; Utility function: return a function of one variable that
;; prints a success message, then returns true.
(define (pass-function name)
  (lambda (r)
    (begin
      (format #t (string-append "All " name " tests passed.~%"))
      #t)))