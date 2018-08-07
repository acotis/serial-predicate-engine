

(load "serial_expansion.scm")


(define (test call expected #!optional (display-anyway #f))
  (let ((result (eval call)))

    (if (equal? expected result)
        (begin (when display-anyway
                     (format #t "Test:     ~a~%" call)
                     (format #t "Result:   ~a~%" result))
               #t)

        (begin (format #t "Test:     ~a~%" call)
               (format #t "Expected: ~a~%" expected)
               (format #t "Result:   ~a~%" result)))))


(test '(is-filled-slot 'c) #t)