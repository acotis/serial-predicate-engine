


(define-syntax perform
  (syntax-rules ()
    ((perform call)
     (begin (format #t "Call:   ~a~%" 'call)
            (format #t "Result: ~a~%" call)))))


(define-syntax test
  (syntax-rules ()
    ((test call expected display-anyway)
     (let ((result call))

       (if (equal? result expected)
           (begin (when display-anyway
                        (format #t "Test:     ~a~%" 'call)
                        (format #t "Result:   ~a~%" result))
                  #t)
             
           (begin (format #t "Test:     ~a~%" 'call)
                  (format #t "Expected: ~a~%" 'expected)
                  (format #t "Result:   ~a~%" result)
                  #f))))

    ((test call expected)
     (test call expected #f))))


(test (+ 3 5) 7)