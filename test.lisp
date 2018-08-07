

(load "serial_expansion.lisp")


(defmacro test (call expected)
  (let ((result (gensym)))
    `(let ((,result (eval ',call)))
       (or (equalp ,result ',expected)
           (progn (format t "~%Call:     ~a~%" ',call)
                  (format t "Expected: ~a~%" ',expected)
                  (format t "Result:   ~a~%" ,result)
                  nil)))))

;(format t "~a~%"
;        (macroexpand
;         '(test (fill-slots '(mai c c) '(ji suq))
;                (mai ji suq))))


(if (and
     
     (test (fill-slots '(mai c c) '(ji suq))
           (mai ji suq))

     (test (fill-slots '(dua ji (leo jado (mai c jado))) '(suq))
           (dua ji (leo jado (mai suq jado))))

     )

    (format t "Tests passed.~%")
  (format t "~%One or more tests failed!~%~%"))