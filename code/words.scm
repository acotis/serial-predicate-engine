#!/usr/bin/guile
!#

;; File:    words.scm
;; Purpose: Provide functions to get the word-forms for a Toaq
;; word.  For example, "dua" should yield a list of the three
;; predicates dua-0, dua-1, and dua-2.

(load "utilities.scm")
(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))

;; Signature getting and setting

(define signatures (make-hash-table))
  
(define (add-word word signature)
  (hash-set! signatures word signature))

(define (get-signature word)
  (or (hash-ref signatures word)
      '()))


;; Word-checking (used at the end of the READ stage)

(define (is-word? word)
  (not (equal? '() (get-signature word))))

(define (get-unknown-words read-output)
  (filter (lambda (x)
            (not (or (is-cmavo? x)
                     (is-word? x))))
          read-output))


;; Make a simple predicate given its name and typelist
;; Create a simple predicate.
;; (make-simple-predicate "leo" '(c 1)) ->
;; '( ((A B)->(leo A B)) c 1 )

(define (make-simple-predicate name typelist)
  (cons (lambda (args)
          (cons name (take args (length typelist))))
        typelist))

(define (make-fail-predicate error)
  (cons (lambda (args)
          (list 'fail error))
        (make-list 0 100)))

(define (is-simple-predicate pred)
  ;;(format #t "(is-simple-predicate ~a)~%" pred)
  (and (procedure? (car pred))
       (every (lambda (k) (or (eq? k 'c) (number? k)))
              (cdr pred))))


;; Take various measurements of a predicate
;;   Typelist:     (typelist leo) = '(c 1)
;;   Canonic form: (gcf leo)      = '(leo A B)

(define (typelist pred)
  (cdr pred))

(define (predicate pred)
  (car pred))

(define (gcf pred)
  ((car pred)
   '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))


;; Make a word given its name (looks up its signature in the
;; hash table)

(define (make-word word)
  (map (lambda (typelist) (make-simple-predicate word typelist))
       (get-signature word)))


;; Load words from the dictionary file

(define (load-words)
  (let ((frames '(("POQ"    . ( () (c) ))
                  ("PAI"    . ( () (c) (c c) ))
                  ("FA"     . ( () (c) (c c) (c c c) ))
                  ("DUA"    . ( () (c) (c 0) ))
                  ("LEO"    . ( () (c) (c 1) ))
                  ("MIA"    . ( () (c) (c 2) ))
                  ("DUATUA" . ( () (c) (c c) (c c 0) ))
                  ("KUOI"   . ( () (c) (c c) (c c 1) ))
                  ("JEQ"    . ( () (c) (c c) (c c 2) ))
                  ("JIA"    . ( () (0) ))
                  ("JIPA"   . ( () (1) ))
                  ("JIE"    . ( () (0) (0 c) ))
                  ("FUI"    . ( () (1) (1 c) ))
                  ("SOQ"    . ( () (c) (c 1) (c 1 c) ))
                  ("MEAKUQ" . ( () (c) (c 0) (c 0 c) ))
                  ("CA"     . ( () (0) (0 0) ))
                  ("JEO"    . ( () (0) (c 1) ))
                  ("CUA"    . ( () (c) (c 1) (c 1 1) ))
                  ("KOE"  . ( () (c) (c c) (c c c) (c c c 1))))))
    
    (format #t "About to open the words file...~%")
    
    (call-with-input-file "../dict/out"
      (lambda (file)
        (while (not (eof-object? (peek-char file)))
               (let* ((line (read-line file))
                      (comma (string-index line #\,))
                      (word (substring line 0 comma))
                      (frame (substring line (+ comma 1)))
                      (assoc (assoc frame frames)))
                 
                 (if assoc
                     (add-word word (cdr assoc)))))))))