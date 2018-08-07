

(load "serial_expansion.lisp")


(defmacro test (call expected &optional (display-anyway nil))
  (let ((result (gensym)))

    (let ((pass-fail ;; binds ,result over its arguments
           (lambda (success failure)
             `(let ((,result ,call))
                (if (equalp ,result ',expected)
                    ,success
                  ,failure))))
          
          (display
           (lambda (success)
             `(progn (format t "Call:     ~a~%" ',call)
                     ,(if success
                          ()
                        `(format t "Expected: ~a~%" ',expected))
                     (format t "Result:   ~a~%~%" ,result)))))

      (funcall pass-fail
               (if display-anyway
                   `(progn ,(funcall display t) t)
                 t)
               `(progn ,(funcall display nil) nil)))))
  
;(format t "~a~%"
;        (macroexpand
;         '(test (fill-slots '(mai c c) '(ji suq))
;                (mai ji suq))))


(if (and

     ;; Test basic slot-filling
     (test (fill-slots '(mai c c) '(ji suq))
           (mai ji suq)
           nil)
     (test (fill-slots '(dua ji (leo jado (mai c jado))) '(suq))
           (dua ji (leo jado (mai suq jado)))
           nil)

     ;; Test basic head-tail expansion
     (test (expand '(dua c 0) '(mai c c))
           (dua c (mai c c))
           t)
     (test (expand '(leo c 1) '(mai c c))
           (leo c (mai jado c))
           t)
     (test (expand '(cheo c 2) '(mai c c))
           (cheo c (mai jado jado))
           t)
     (test (expand '(du 0) '(mai c c))
           (du (mai c c))
           t)
     (test (expand '(soq c 1 c) '(de c))
           (soq c (de jado) c)
           t)
     (test (expand '(soq c 1 c) '(dua c 0))
           (soq c (dua jado 0) c)
           t)
     (test (expand '(soq c 1 c) '(leo c 1))
           (soq c (leo jado 1) c)
           t)
     
     )
    (format t "Tests passed.~%")
  (format t "One or more tests failed!~%~%"))