#!/usr/bin/guile
!#

(use-modules (ice-9 rdelim))


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
          (cmd cmd)
          (#t '()))))


;; Read whole test-input files

(define (read-test-input-file filename)
  (let* ((f (open-input-file filename))
         (lines
          
          (do ((line (read-line f) (read-line f))
               (collect '()))
               
              ((eof-object? line)
               collect)

            (set! collect
                  (append collect
                          (list (parse-line line)))))))
    
    (close-input-port f)
    
    (filter (lambda (s) (not (null? s))) lines)))


(format #t "~a~%" (read-test-input-file "full-tests-input.txt"))