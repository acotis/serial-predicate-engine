

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
             `(progn (format t "Test:     ~a~%" ',call)
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
     (test (expand-binary '(dua c 0) '(mai c c))
           (dua c (mai c c))
           nil)
     (test (expand-binary '(leo c 1) '(mai c c))
           (leo c (mai jado c))
           nil)
     (test (expand-binary '(cheo c 2) '(mai c c))
           (cheo c (mai jado jado))
           nil)
     (test (expand-binary '(du 0) '(mai c c))
           (du (mai c c))
           nil)
     (test (expand-binary '(soq c 1 c) '(de c))
           (soq c (de jado) c)
           nil)
     (test (expand-binary '(soq c 1 c) '(dua c 0))
           (soq c (dua jado 0) c)
           nil)
     (test (expand-binary '(soq c 1 c) '(leo c 1))
           (soq c (leo jado 1) c)
           nil)

     ;; Prep for poly-expansion
     (test (expand-binary '(hica 1 1) '(dua c 0))
           (hica 1 (dua jado 0))
           nil)
     (test (expand-binary '(soq c 1 c) '(hica 1 (dua jado 0)))
           (soq c (hica jado (dua jado 0)) c)
           nil)
     (test (expand-binary '(seqkai 1) '(dua c 0)) ;; selkai
           (seqkai (dua jado 0))
           nil)
     (test (expand-binary '(soq c 1 c) '(seqkai (dua jado 0)))
           (soq c (seqkai (dua jado jado)) c)
           nil)

     ;; Basic poly-expansion
     (test (expand '((dua c 0) (mai c c)))
           (dua c (mai c c))
           nil)
     (test (expand '((dua c 0) (leo c 1) (mai c c)))
           (dua c (leo c (mai jado c)))
           nil)
     (test (expand '((soq c 1 c) (hica 1 1) (dua c 0)))
           (soq c (hica jado (dua jado 0)) c)
           nil)
     (test (expand '((soq c 1 c) (seqkai 1) (dua c 0)))
           (soq c (seqkai (dua jado jado)) c)
           nil)
     
     )
    (format t "Tests passed.~%")
  (format t "One or more tests failed!~%~%"))