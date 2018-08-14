#!/usr/bin/guile
!#

(load "predicates.scm")
(load "words.scm")
(load "pretty-print.scm")
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


;; (and) but with no short-circuiting
(define-syntax and-all
  (syntax-rules ()
    ((and-all exp ...)
     (let ((all #t))
       (begin (if (not exp) (set! all #f))
              ...
              all)))))


;; Run a series of tests.  Run all tests even after one fails.
(define-syntax run-tests
  (syntax-rules ()
    ((run-tests ((call expected) ...)
                display-anyway
                on-failure
                on-success)

     (if (and-all (test call expected display-anyway)
                  ...)
         on-success
         on-failure))

    ((run-tests ((call expected) ...)
                display-anyway
                on-failure)
     (run-tests ((call expected) ...)
                display-anyway
                on-failure
                '()))))


;; Which tests to display even on success

(define show-gxf #f)
(define show-jado-ify #f)
(define show-expand-binary #f)
(define show-expand #f)
(define show-basic-words #f)
(define show-pretty-print #t)

(define show-compose-binary #t)
(define show-compose #t)
(define show-compose-words #t)

;; Some sample predicates

(define jai-0  (make-simple-predicate "jai"  '()))
(define jai-1  (make-simple-predicate "jai"  '(c)))
(define gi-1   (make-simple-predicate "gi"   '(0)))
(define mai-2  (make-simple-predicate "mai"  '(c c)))

(define dua-2  (make-simple-predicate "dua"  '(c 0)))
(define tua-2  (make-simple-predicate "tua"  '(c 0)))

(define kuai-2 (make-simple-predicate "kuai" '(c 1)))
(define leo-2  (make-simple-predicate "leo"  '(c 1)))
(define jeaq-2 (make-simple-predicate "jeaq" '(c 1)))
      
(define soq-3  (make-simple-predicate "soq"  '(c 1 c)))


;; (gcf) "Get canonic form"
;; (gtf) "Get type form"
;; (gff) "Get full form

(run-tests
 ( ((gcf mai-2) '("mai" A B))
   ((gcf dua-2) '("dua" A B))
   ((gcf soq-3) '("soq" A B C))
   ((gcf jai-0) '("jai"))

   ((gtf mai-2) '("mai" c c))
   ((gtf dua-2) '("dua" c 0))
   ((gtf soq-3) '("soq" c 1 c))
   ((gtf jai-0) '("jai"))

   ((gff mai-2) '((c c) ("mai" A B)))
   ((gff dua-2) '((c 0) ("dua" A B)))
   ((gff soq-3) '((c 1 c) ("soq" A B C)))
   ((gff jai-0) '(() ("jai"))) )

 show-gxf
 (begin (format #t "One or more (gxf) tests failed.~%")
        (quit)))


;; (jado-ify) tests

(let ((mai-2/1 (jado-ify mai-2 1))
      (dua-2/2 (jado-ify dua-2 2))
      (soq-3/1 (jado-ify soq-3 1))
      (soq-3/0 (jado-ify soq-3 0)))

  (run-tests
   ( ((gff mai-2/1)
      '((c) (li ((jado 1)) ("mai" ( do 1) A))))
     
     ((gff dua-2/2)
      '(() (li ((jado 1) (jado 2)) ("dua" ( do 1) ( do 2)))))
     
     ((gff soq-3/1)
      '((1 c) (li ((jado 1)) ("soq" ( do 1) A B))))
     
     ((gff soq-3/0)
      '((c 1 c) ("soq" A B C))) )
      
   show-jado-ify
   (begin (format #t "One or more (jado-ify tests) failed.~%")
          (quit))))
        
;; expand-binary and expand
(let ((dua-mai (expand-binary dua-2 mai-2))
      (leo-mai (expand-binary leo-2 mai-2))
      (soq-dua (expand-binary soq-3 dua-2))
      (soq-gi  (expand-binary soq-3 gi-1))
      (dua-jai (expand-binary dua-2 jai-0))
      (ktjj (expand (list kuai-2 tua-2 jeaq-2 jai-1))))
  
  (run-tests 
   ( ((gff dua-mai)
      '((c c c) ("dua" A ("mai" B C))))
          
     ((gff leo-mai)
      '((c c) ("leo" A (li ((jado 1)) ("mai" ( do 1) B)))))
     
     ((gff soq-dua)
      '((c 0 c) ("soq" A (li ((jado 1)) ("dua" ( do 1) B)) C)))

     ((gff soq-gi)
      '((c c) ("soq" A (li ((jado 1)) ("gi" ( do 1))) B)))
     
     ((gff dua-jai)
      '((c) ("dua" A ("jai")))) )

   show-expand-binary
   (begin (format #t
                  "One or more (expand-binary) tests failed.~%")
          (quit)))

  (run-tests
   ( ((gff ktjj)
      '((c c)
        ("kuai" A
         (li ((jado 2))
             ("tua" ( do 2)
              ("jeaq" B
               (li ((jado 1)) ("jai" ( do 1))))))))) )
   
   show-expand
   (begin (format #t "One or more (expand) tests failed.~%")
          (quit))))

;; compose-words

(add-word "maomao" '(() (c)))
(add-word "kune"   '(() (c)))
(add-word "jai"    '(() (c)))
(add-word "de"     '(() (c)))

(add-word "mai"    '(() (c) (c c)))
(add-word "pai"    '(() (c) (c c)))

(add-word "dua"    '(() (c) (c 0)))
(add-word "shao"   '(() (c) (c 0)))
(add-word "tua"    '(() (c) (c 0)))

(add-word "kuai"   '(() (c) (c 1)))
(add-word "leo"    '(() (c) (c 1)))
(add-word "jeaq"   '(() (c) (c 1)))

(add-word "du"     '(() (0) (c 1)))
(add-word "jeo"    '(() (0) (c 1)))
(add-word "bu"     '(() (0) (c 1)))

(add-word "soq"    '(() (c) (c 1) (c 1 c)))


;; Basic word tests
(run-tests
 ( ((wgff (make-word "maomao"))
    '((()  ("maomao"))
      ((c) ("maomao" A))))
   
   ((wgff (make-word "kuai"))
    '((()    ("kuai"))
      ((c)   ("kuai" A))
      ((c 1) ("kuai" A B))))
   
   ((wgff (make-word "du"))
    '((()    ("du"))
      ((0)   ("du" A))
      ((c 1) ("du" A B)))) )
   
 show-basic-words

 (begin (format #t "One or more basic word tests failed.~%")
        (quit)))


;; Pretty-print tests

;; Temp functions to make this part easier

;; Make-word
(define (mp word args)
  (list-ref (make-word word) args))

;; Make-serial-predicate
(define (msp ls)
  (gcf (expand (map (lambda (p) (mp (car p) (cadr p))) ls))))

(run-tests
 ( ((cf->string (msp '(("kuai" 2) ("maomao" 1))) 4)
    "kủai A lî ja dó[1] bi mảomao dó[1] na na")

   ((cf->string (msp '(("kuai" 2) ("tua" 2) ("jeaq" 2) ("jai" 1))) 4)
    "kủai A lî ja dó[2] bi tủa dó[2] jêaq B lî ja dó[1] bi jải dó[1] na na na na")

   ((cf->string (msp '(("kuai" 2) ("soq" 3) ("de" 1))) 4)
    "kủai A lî ja dó[2] bi sỏq dó[2] lî ja dó[1] bi dẻ dó[1] na B na na")
   )

 show-pretty-print

 (begin (format #t "One or more pretty-print tests failed.~%")
        (quit)))
    


(format #t "All tests passed.~%")