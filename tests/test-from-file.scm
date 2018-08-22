#!/usr/bin/guile
!#

(load "../new-code/utilities.scm")
(load "test-macro.scm")
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))


;; Generic line-processing functions:
;;   - Remove comments
;;   - Get the next space-separated word from a string
;;   - Parse into a command and some args


;; Return index of beginning of first substring of string
;; to satisfy fun
(define (string-find-first-tail fun string)
  (if (or (equal? "" string) (fun string))
      0
      (+ 1 (string-find-first-tail fun (substring string 1)))))

;; Remove everything after and including the first "//"
(define (remove-comments string)
  (substring
   string
   0
   (string-find-first-tail
    (lambda (s)
      (and (>= (string-length s) 2)
           (equal? "//" (substring s 0 2))))
    string)))

;; Remove the tab characters from an expected-output line
(define (clean-tabs line)
  (let ((i (string-find-first-tail
            (lambda (s)
              (equal? #\= (string-ref s 0)))
            line)))
    
    (string-append (string-trim-both
                    (substring line 0 i))
                   " = "
                   (string-trim-both
                    (substring line (+ i 1))))))

;; Read up to the first space, return the first word cons'd
;; to the left-trim of everything after it
(define (next-word string)
  (let ((first-space
         (string-find-first-tail
          (lambda (s)
            (equal? #\Space (string-ref s 0)))
          string)))

    (cons (substring string 0 first-space)
          (string-trim (substring string first-space)))))

;; ex ":test la la la"    -> '(test "la la la")
;; ex ":next // la la la" -> 'next
(define (parse-line line)
  (let* ((pline (string-trim-both (remove-comments line)))
         (pair (next-word pline))
         (cmdstr (car pair))
         (rest (cdr pair))
         (cmd-assoc (assoc cmdstr '((":test" . test)
                                    (":next" . next)
                                    (":end" . end)
                                    (":skip" . skip))))
         (cmd (if cmd-assoc (cdr cmd-assoc) '())))

    (cond ((equal? pline "") '())
          ((equal? cmd 'test) (list 'test rest))
          ((not (null? cmd)) cmd)
          (#t "tc")))) ;;(clean-tabs pline)))))


;; Read whole files

;; Parse all the lines in a file, remove empty parses
(define (read-whole-file filename)
  (call-with-input-file filename
    (lambda (f)

      (filter
       (lambda (s) (not (null? s)))

       (do ((line (read-line f) (read-line f))
            (collect '()
                     (append collect (list (parse-line line)))))
           
           ((eof-object? line)
            collect))))))

;; Return a list of test-case-input strings
(define (read-test-input-file filename)
  (map cadr (read-whole-file filename)))


;; Grab the next 'skip or ('test ... 'end) off of a list
(define (next-case lines)
  (if (eq? 'skip (car lines))
      (list 'skip (cdr lines))
      
      (let* ((i (find-first (lambda (k) (eq? k 'end)) lines))
             (next (take lines i)))

        (list (cdr next)
              (drop lines (+ i 1))))))


;; Return a list of lists of test-case-output strings
(define (read-test-output-file filename)
  (let ((lines (read-whole-file filename))
        (collect '()))

    (do ()
        ((null? lines)
         collect)

      (let ((split (next-case lines)))
        (set! collect (append collect (list (car split))))
        (set! lines (cadr split))))))
      

;; Create test cases from an input and output file

(define (create-cases-from-files infile outfile)
  (let ((in (read-test-input-file infile))
        (out (read-test-output-file outfile)))
    (filter (lambda (tc)
              (not (eq? (cadr tc) 'skip)))
            (map list in out))))


;; (format #t "~a~%" (read-test-input-file "full-tests-input.txt"))

;; (let ((expecteds
;;        (read-test-output-file "full-tests-nofilter.txt")))
;;        ;;(read-test-output-file "dummy.txt")))
       
;;   (map (lambda (e)
;;          (map (lambda (n)
;;                 (format #t "~a~%" n))
;;               (if (equal? e 'skip) '(skip) e))
;;          (format #t "~%"))
;;        expecteds))



;; (let ((cases (create-cases-from-files
;;               "full-tests-input.txt"
;;               "full-tests-nofilter.txt")))

;;   (map (lambda (tc) (format #t "~a~%~%" tc)) cases))
   
;; (quit)



;; Test an input file against an output file, given the name
;; of the function to test

(define-macro (test-from-files infile outfile function-name
                               display-anyway fail-fun pass-fun)
  
  `(run-tests ,(map (lambda (tc)
                      (let ((result (list (list function-name
                                                (car tc))
                                          (cons 'quote
                                                (cdr tc)))))
                        (format #t "tc = *~a*~%" tc)
                        (format #t "result = *~a*~%" result)
                        result))
                        
                    (create-cases-from-files infile outfile))
              
              ,display-anyway
              ,fail-fun
              ,pass-fun))