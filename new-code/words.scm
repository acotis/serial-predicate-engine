#!/usr/bin/guile
!#

;; File:    words.scm
;; Purpose: Provide functions to get the word-forms for a Toaq
;; word.  For example, "dua" should yield a list of the three
;; predicates dua-0, dua-1, and dua-2.


;; Signature getting and setting

(define signatures (make-hash-table))
  
(define (add-word word typelist)
  (hash-set! signatures word typelist))

(define (get-signature word)
  (or (hash-ref signatures word)
      '()))


;; Make a simple predicate given its name and typelist
;; Create a simple predicate.
;; (make-simple-predicate "leo" '(c 1)) ->
;; '( ((A B)->(leo A B)) c 1 )

(define (make-simple-predicate name typelist)
  (cons (lambda (args)
          (cons name args))
        typelist))

(define (is-simple-predicate pred)
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
   (take '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
         (length (cdr pred)))))



;; Make a word given its name (looks up its signature in the
;; hash table)

(define (make-word word)
  (map (lambda (typelist) (make-simple-predicate word typelist))
       (get-signature word)))


;; Add words to the dictionary by frame

(map (lambda (pair)
       (map (lambda (word)
              (add-word word (car pair)))
            (cadr pair)))

        ;; JEO frame
     '( ((() (0) (c 1))
         ("jeo" "bu" "ceo"))

        ;; POQ frame
        ((() (c))
         ("poq" "jai" "nuo" "de" "fie" "tea" "meo"
          "maomao" "riofa" "tishagiq"))

        ;; PAI frame
        ((() (c) (c c))
         ("pai" "mai" "ti" "chuq" "bai"))

        ;; FA frame
        ((() (c) (c c) (c c c))
         ("fa"))
        
        ;; GI frame
        ((() (0))
         ("gi" "hui"))
        
        ;; DUA frame
        ((() (c) (c 0))
         ("dua" "tua"))

        ;; KUAI frame
        ((() (c) (c 1))
         ("kuai" "leo" "jeaq" "kea"))

        ;; CUA frame
        ((() (c) (c 1) (c 1 1))
         ("cua" "dui"))

        ;; SOQ frame
        ((() (c) (c 1) (c 1 c))
         ("soq"))
        
        ;; CHEO frame
        ((() (c) (c 2))
         ("cheo" "mia"))
        ))