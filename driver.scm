
(load "serial_expansion.scm")
(use-syntax (ice-9 syncase))


(define-syntax perform
  (syntax-rules ()
    ((test call)
     (begin (format #t "Call:   ~a~%" 'call)
            (format #t "Result: ~a~%" call)))))

(perform (fill-slots '(mai c c) '(ji suq)))
