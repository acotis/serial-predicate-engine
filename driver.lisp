

(load "serial_expansion.lisp")


;; Print out a call, then print out the result of the call

(defmacro perform (call)
  `(progn
     (format t "Call:   ~a~%" ',call)
     (format t "Result: ~a~%~%" ,call)))


