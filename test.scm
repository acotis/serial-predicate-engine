#!/usr/bin/guile
!#

(load "predicates.scm")
(load "words.scm")
(use-syntax (ice-9 syncase))


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


(let ((show-gcf #f)  ;; Show (gcf) "get canonic form" tests
      (show-jdf #f)  ;; Show (jado-ify) tests
      (show-eb #t)   ;; Show (expand-binary) tests
      (show-ex #t)   ;; Show (expand) tests

      (jai-0 (make-simple-predicate "jai" '()))
      (jai-1 (make-simple-predicate "jai" '(c)))
      (gi-1 (make-simple-predicate "gi" '(0)))
      (mai-2 (make-simple-predicate "mai" '(c c)))

      (dua-2 (make-simple-predicate "dua" '(c 0)))
      (tua-2 (make-simple-predicate "tua" '(c 0)))
      
      (kuai-2 (make-simple-predicate "kuai" '(c 1)))
      (leo-2 (make-simple-predicate "leo" '(c 1)))
      (jeaq-2 (make-simple-predicate "jeaq" '(c 1)))
      
      (soq-3 (make-simple-predicate "soq" '(c 1 c))))
      
  (if (and
       
       ;; gcf
       (test (gcf mai-2) '("mai" A B) show-gcf)
       (test (gcf dua-2) '("dua" A B) show-gcf)
       (test (gcf soq-3) '("soq" A B C) show-gcf)
       (test (gcf jai-0) '("jai") show-gcf)

       ;; jado-ify
       (test (gcf (jado-ify mai-2 1))
             '("mai" jado A)
             show-jdf)
       (test (gcf (jado-ify dua-2 2))
             '("dua" jado jado)
             show-jdf)
       (test (gcf (jado-ify soq-3 1))
             '("soq" jado A B)
             show-jdf)
       (test (gcf (jado-ify soq-3 0))
             '("soq" A B C)
             show-jdf)
       (test (cdr (jado-ify soq-3 1))
             '(1 c)
             show-jdf)

       ;; expand-binary and expand
       (let ((dua-mai (expand-binary dua-2 mai-2))
             (leo-mai (expand-binary leo-2 mai-2))
             (soq-dua (expand-binary soq-3 dua-2))
             (soq-gi  (expand-binary soq-3 gi-1))
             (dua-jai (expand-binary dua-2 jai-0))
             (ktjj (expand (list kuai-2 tua-2 jeaq-2 jai-1))))
         (and 
          (test (gcf dua-mai) '("dua" A ("mai" B C))     show-eb)
          (test (typelist dua-mai) '(c c c)              show-eb)
          
          (test (gcf leo-mai) '("leo" A ("mai" jado B))  show-eb)
          (test (typelist leo-mai) '(c c)                show-eb)
          
          (test (gcf soq-dua) '("soq" A ("dua" jado B) C)show-eb)
          (test (typelist soq-dua) '(c 0 c)              show-eb)

          (test (gcf soq-gi)  '("soq" A ("gi" jado) B)   show-eb)
          (test (typelist soq-gi)  '(c c)                show-eb)
          
          (test (gcf dua-jai) '("dua" A ("jai"))         show-eb)
          (test (typelist dua-jai) '(c)                  show-eb)

          (test (gcf ktjj)
                '("kuai" A ("tua" jado ("jeaq" B ("jai" jado))))
                show-ex)
          (test (typelist ktjj) '(c c) show-ex)
         ))
       )
      
      (format #t "All tests passed.~%")
      (format #t "One or more tests failed!~%")))