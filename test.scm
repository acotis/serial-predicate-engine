#!/usr/bin/guile
!#

(load "serial_expansion.scm")
(use-syntax (ice-9 syncase))
(use-modules (ice-9 pretty-print))


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



(let ((show-fsh #f) ;; Show fill-slots helpers tests
      (show-fs #f)  ;; Show fill-slots test
      (show-eb #f)  ;; Show expand-binary tests
      (show-pep #f) ;; Show poly-predicate prep tests
      (show-pe #t)) ;; Show poly-predicate tests
      
  (if (and
       
       ;; fill-slots helper functions
       (test (has-open-slot 'c) #t show-fsh)
       (test (has-open-slot 3) #t show-fsh)
       (test (has-open-slot 'suq) #f show-fsh)
       (test (has-open-slot '(dua c 0)) #t show-fsh)
       (test (has-open-slot '(dua jado (mai ji c))) #t show-fsh)
       (test (has-open-slot '(dua jado (mai ji ho))) #f show-fsh)
       (test (fill-one-slot '(mai c c) 'ji)
             '(mai ji c)
             show-fsh)
       
       ;; fill-slots function
       (test (fill-slots '(mai c c) '(ji suq))
             '(mai ji suq)
             show-fs)
       (test (fill-slots '(dua ji (leo ho (mai c jado))) '(suq))
             '(dua ji (leo ho (mai suq jado)))
             show-fs)
       
       ;; expand-binary function
       (test (expand-binary '(dua c 0) '(mai c c))
             '(dua c (mai c c))
             show-eb)
       (test (expand-binary '(leo c 1) '(mai c c))
             '(leo c (mai jado c))
             show-eb)
       (test (expand-binary '(cheo c 2) '(mai c c))
             '(cheo c (mai jado jado))
             show-eb)
       (test (expand-binary '(du 0) '(mai c c))
             '(du (mai c c))
             show-eb)
       (test (expand-binary '(soq c 1 c) '(de c))
             '(soq c (de jado) c)
             show-eb)
       (test (expand-binary '(soq c 1 c) '(dua c 0))
             '(soq c (dua jado 0) c)
             show-eb)
       (test (expand-binary '(soq c 1 c) '(leo c 1))
             '(soq c (leo jado 1) c)
             show-eb)

       ;; Prep for poly-expansion
       (test (expand-binary '(hica 1 1) '(dua c 0))
             '(hica 1 (dua jado 0))
             show-pep)
       (test (expand-binary '(soq c 1 c) '(hica 1 (dua jado 0)))
             '(soq c (hica jado (dua jado 0)) c)
             show-pep)
       (test (expand-binary '(seqkai 1) '(dua c 0)) ;; selkai
             '(seqkai (dua jado 0))
             show-pep)
       (test (expand-binary '(soq c 1 c) '(seqkai (dua jado 0)))
             '(soq c (seqkai (dua jado jado)) c)
             show-pep)
       
       ;; Basic poly-expansion
       (test (expand '((dua c 0) (mai c c)))
             '(dua c (mai c c))
             show-pe)
       (test (expand '((dua c 0) (leo c 1) (mai c c)))
             '(dua c (leo c (mai jado c)))
             show-pe)
       (test (expand '((soq c 1 c) (hica 1 1) (dua c 0)))
             '(soq c (hica jado (dua jado 0)) c)
             show-pe)
       (test (expand '((soq c 1 c) (seqkai 1) (dua c 0)))
             '(soq c (seqkai (dua jado jado)) c)
             show-pe)
       
       )
      
      (format #t "All tests passed.~%")
      (format #t "One or more tests failed!~%")))
  
