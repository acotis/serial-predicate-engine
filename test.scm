

(load "serial_expansion.scm")


(define-syntax test
  (syntax-rules ()
    
    ((test call expected display-anyway)
     (let ((result (eval call)))  
       (if (equal? expected result)
           (begin (when display-anyway
                        (format #t "Test:     ~a~%" 'call)
                        (format #t "Result:   ~a~%" result))
                  #t)
           
           (begin (format #t "Test:     ~a~%" 'call)
                  (format #t "Expected: ~a~%" expected)
                  (format #t "Result:   ~a~%" result)
                  #f))))

    ((test call expected)
     (test call expected #f))))
    

(test (+ 3 5) 8)