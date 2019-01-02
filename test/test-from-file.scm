#!/usr/bin/guile
!#

(load "../code/utilities.scm")
(load "test-macro.scm")
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))


;; Generic line-processing functions:
;;   - Remove comments
;;   - Get the next space-separated word from a string
;;   - Parse into a command and some args


;; Return everything before the first instance of pattern

(define (string-take-before string pattern)
  (cond ((< (string-length string) (string-length pattern))
         string)
        
        ((equal? pattern
                 (substring string 0 (string-length pattern)))
         "")
        
        (#t
         (string-append (substring string 0 1)
                        (string-take-before (substring string 1)
                                            pattern)))))

;; Split on a pattern

(define (string-cut string pattern)
  (let* ((before (string-take-before string pattern))
         (after  (substring string (string-length before))))
    (list (string-trim-both before)
          (string-trim-both after))))

;; Remove comments

(define (remove-comments line)
  (string-trim-both (string-take-before line "//")))


;; Parse all the lines in a file, remove empty lines

(define (read-whole-file filename)
  (call-with-input-file filename
    (lambda (file)
      ;; Necessary to read unicode correctly
      (set-port-encoding! file "UTF-8") 
      
      (filter
       (lambda (s) (not (equal? s "")))

       (do ((line (read-line file) (read-line file))
            (collect '()
                     (append collect
                             (list (remove-comments line)))))
           
           ((eof-object? line)
            collect))))))

        
;; Create test cases from an input and output file

(define (create-cases-from-files infile outfile)
  (let ((in (read-whole-file infile))
        (out (read-whole-file outfile)))
    (filter (lambda (tc)
              (not (equal? (cadr tc) ":skip")))
            (map list in out))))


;; Test an input file against an output file, given the name
;; of the function to test.

(define-macro (test-from-files infile outfile parse-fun-name
                               display-anyway fail-fun pass-fun)
  `(run-tests
    
    ,(map (lambda (test-case)
            `((car (,parse-fun-name ,(car test-case)))
              ,(cadr test-case)))
         
         (create-cases-from-files infile outfile))
    
    ,display-anyway
    ,fail-fun
    ,pass-fun))