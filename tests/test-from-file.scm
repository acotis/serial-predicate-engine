#!/usr/bin/guile
!#

(load "../new-code/utilities.scm")
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

;; ex ":test la la la" -> '(test "la la la")
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
          (#t pline)))) ;;pline))))


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

        (format #t "lines = ~a~%" lines)
        (format #t "next = ~a~%" next)
        
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
      


(format #t "~a~%" (read-test-input-file "full-tests-input.txt"))

(format #t "~%~%~a~%" (read-whole-file "full-tests-nofilter.txt"))

(format #t "~%~%~a~%"
        (read-test-output-file "full-tests-nofilter.txt"))