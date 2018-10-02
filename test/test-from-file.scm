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
        
        ((equal? pattern (substring string 0 (string-length
                                              pattern)))
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


;; If the line begins with a keyword, pull it out
;;   + ":test la la la"    -> '(test "la la la")
;;   + ":next // la la la" -> '(next "")
;;   + "pai A B"           -> '(line "pai a b")
;;   + ""                  -> '(line "")

(define (parse-line line)
  (let* ((pline (remove-comments line))
         (pair (string-cut pline " "))
         (cmdstr (car pair))
         (rest (cadr pair))
         (cmd-assoc (assoc cmdstr '((":test" . test)
                                    (":next" . next)
                                    (":end"  . end)
                                    (":skip" . skip)))))

    (if cmd-assoc
        (list (cdr cmd-assoc) rest)
        (list 'line pline))))


;; Parse all the lines in a file, remove empty parses

(define (read-whole-file filename)
  (call-with-input-file filename
    (lambda (f)

      (filter
       (lambda (s) (not (equal? s '(line ""))))

       (do ((line (read-line f) (read-line f))
            (collect '()
                     (append collect (list (parse-line line)))))
           
           ((eof-object? line)
            collect))))))


;; Return a list of test-case-input strings

(define (read-test-input-file filename)
  (map cadr (read-whole-file filename)))


;; Grab the next '(skip "") or '((next "") ... (end ""))
;; (Assumes a properly-formatted file)

(define (next-case lines)
  (cond ((eq? (car (car lines)) 'skip) 'skip)
        ((eq? (car (car lines)) 'end) (list (car lines)))
        (#t (append (list (car lines))
                    (next-case (cdr lines))))))

;; Return a list of lists of test-case-output strings

(define (read-test-output-file filename)
  (unfold null?
          (lambda (lines)
            (let ((next (next-case lines)))
              (if (eq? 'skip next)
                  'skip
                  (cdr (drop-right (next-case lines) 1)))))

          (lambda (lines)
            (let ((next (next-case lines)))
              (drop lines
                    (if (eq? 'skip next) 1 (length next)))))
      
          (read-whole-file filename)))
        
;; Create test cases from an input and output file

(define (create-cases-from-files infile outfile)
  (let ((in (read-test-input-file infile))
        (out (read-test-output-file outfile)))
    (filter (lambda (tc)
              (not (eq? (cadr tc) 'skip)))
            (map list in out))))


;; Test an input file against an output file, given the name
;; of the function to test.

(define-macro (test-from-files infile outfile function-name
                               display-anyway fail-fun pass-fun)
  `(run-tests
    
    ,(fold append
           (map (lambda (test-case)
                  (map (lambda (n)
                         `((list-ref (,function-name
                                      ,(car test-case))
                                     ,(+ n 1)) ;; Skip blank line
                           ,(cadr (list-ref(cadr test-case) n))))
                       
                       (iota (length (cadr test-case)))))
                (create-cases-from-files infile outfile)))
    
    ,display-anyway
    ,fail-fun
    ,pass-fun))