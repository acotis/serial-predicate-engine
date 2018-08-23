#!/usr/bin/guile
!#

(use-modules (srfi srfi-1))
(load "utilities.scm")
(load "words.scm")


(define marked-vowels
  '(( "a" . ("ā" "á" "ǎ" "ả" "â" "à" "ã" "a") )
    ( "e" . ("ē" "é" "ě" "ẻ" "ê" "è" "ẽ" "e") )
    ( "i" . ("ī" "í" "ǐ" "ỉ" "î" "ì" "ĩ" "i") )
    ( "o" . ("ō" "ó" "ǒ" "ỏ" "ô" "ò" "õ" "o") )
    ( "u" . ("ū" "ú" "ǔ" "ủ" "û" "ù" "ũ" "u") )))

;; Add a diacritic mark to a single vowel, passed as a string

(define (add-diacritic vowel tone)
  (list-ref (assoc vowel marked-vowels) tone))


;; Add a tone marking to the appropriate letter of a single word,
;; passed as a string

(define (add-tone word tone)
  (fold string-append
        (replace-first (map (lambda (n)
                              (substring word n (+ n 1)))
                            (iota (string-length word)))
                       (lambda (l)
                         (or (equal? l "a") (equal? l "e")
                             (equal? l "i") (equal? l "o")
                             (equal? l "u")))
                       (lambda (v) (add-diacritic v tone)))))


;; Return a printable form for a (do #n) or (jado #n)
;; expression, given n

(define (printable-do n)
  (string-append (add-tone "do" 2)
                 "["
                 (number->string n)
                 "]"))

(define (printable-jado n)
  (string-append "ja " (printable-do n)))

(define (printable-do-jado exp)
  (if (= 2 (length exp)) ;; (jado n) / (do n) case
      (if (eq? (car exp) 'jado)
          (printable-jado (cadr exp))
          (printable-do (cadr exp)))
      (string-append "ja " (add-tone "do" 2)))) ;; (jado) case


;; Convert a whole canonic form plus a tone to a printable form

(define (append-with-spaces a b)
  (string-append a " " b))

(define (cf->string cf tone)
  (cond ((symbol? cf) ;; c, 0, 1, 2, A, B, ...
         (symbol->string cf))

        ;; (do 1) and (jado 1)
        ((or (eq? (car cf) 'do) (eq? (car cf) 'jado))
         (printable-do-jado cf))
        
        ;; (li ((jado 1) (jado 2)) (...))
        ((eq? (car cf) 'li)
         (fold append-with-spaces
               (append (list (add-tone "li" tone))
                       (map printable-do-jado (cadr cf))
                       (list "bi")
                       (list (cf->string (caddr cf) 4)))))

        ;; (pred args...) or (lu to RU ... to)
        (#t
         (let ((stone (if (equal? (car cf) "lu") 4 5)))
           (fold append-with-spaces
                 (append (list (add-tone (car cf) tone))
                         (map (lambda (a) (cf->string a stone))
                              (cdr cf))
                         (list "na")))))))


;; Pretty-print a whole predicate
;; Example: <[c 0] (dủa A B)>

(define (remove-trailing-na str)
  (let ((len (string-length str)))
    (if (equal? "na" (substring str (- len 2)))
        (remove-trailing-na
         (string-trim-both (substring str 0 (- len 2))))
        str)))

(define (pred->string pred)
  (string-append "<["
                 (if (not (null? (typelist pred)))
                     (fold append-with-spaces
                           (map (lambda (type)
                                  (if (number? type)
                                      (number->string type)
                                      (symbol->string type)))
                                (typelist pred)))
                     "")
                 "] ("
                 (remove-trailing-na
                  (cf->string (gcf pred) 4))
                 ")>"))