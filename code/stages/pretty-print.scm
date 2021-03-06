#!/usr/bin/guile
!#

;; File:    pretty-print.scm
;; Purpose: Turn a list of predicates into a list of pretty
;;          output strings.

;; Input:  ( <lu to ru (lu to ru (gi) to (pai)) to (hui)>
;;           <lu to ru (gi (pai)) to (hui)>  … )
;; Output: ( "lủ to ru lủ to ru gỉ na to pải na na to hủi"
;;           "lủ to ru gỉ pâi na na to hủi" … ) 


(load "../utilities.scm")
(load "../words.scm")
(use-modules (srfi srfi-1))


(define marked-vowels
  '(( #\a . "āáǎảâàãa" )
    ( #\e . "ēéěẻêèẽe" )
    ( #\i . "īíǐỉîìĩı" )
    ( #\o . "ōóǒỏôòõo" )
    ( #\u . "ūúǔủûùũu" )))

(define vowel?
  (let ((vowels (map car marked-vowels)))
    (lambda (v)
      (member v vowels))))

;; Add a diacritic mark to a single vowel, passed as a string

(define (add-diacritic vowel tone)
  (string-ref (cdr (assv vowel marked-vowels)) (1- tone)))


;; Add a tone marking to the appropriate letter of a single word,
;; passed as a string

(define (add-diacritics* letters tone)
  (let ((first-vowel (list-index vowel? letters)))
    (if (not first-vowel) letters
      (let* ((onset (take letters first-vowel))
             (rime (drop letters first-vowel))
             (first-consonant (list-index (lambda (l)
                                                  (not (vowel? l)))
                                          rime)))
            (append onset
                    (cons (add-diacritic (car rime) tone)
                          (if first-consonant
                              (append
                                (take (cdr rime)
                                      (1- first-consonant))
                                (add-diacritics*
                                  (drop rime first-consonant)
                                  1))
                              (cdr rime))))))))

(define (add-diacritics los tone)
  (if (list? los)
    (add-diacritics* los tone)
    (list->string (add-diacritics* (string->list los) tone))))

(define (add-tone word tone)
  (list->string
    (add-diacritics (string->list word)
                    tone)))


;; Uakci's addition: convert number to its Toaq compound form. This is
;; to facilitate friendly dó variables, e.g., dóshī, dógū, etc.

;; Count from one to nine.
(define (digit->toaq n)
  (let ((numbers '(""   "shi" "gu"   "saq"  "jo"
                   "fe" "ci"  "diai" "roai" "nei")))
       (list-ref numbers n)))

(define (number->toaq n)
  (when (not (and (>= n 0) (< n 1e6) (integer? n)))
        (error "sorry, out of range"))
  (cond
    ((zero? n) "")
    ((>= n 1000)
      (string-append (number->toaq (quotient n 1000))
                     "biq"
                     (number->toaq (remainder n 1000))))
    (else 
      (let* ((hundreds (quotient n 100))
             (tens (quotient (remainder n 100) 10))
             (ones (remainder n 10)))
            (string-append
              (digit->toaq hundreds)
                (if (zero? hundreds) "" "fue")
              (digit->toaq tens)
                (if (zero? tens)     "" "hei")
              (digit->toaq ones))))))

;; Return a printable form for a (do #n) or (jado #n)
;; expression, given n

(define (printable-do n)
  (add-tone (string-append "do" (number->toaq n)) 2))

(define (printable-jado n)
  (string-append "ja " (printable-do n)))

(define (printable-do-jado exp)
  (if (= 2 (length exp)) ;; (jado n) / (do n) case
      (if (eq? (car exp) 'jado)
          (printable-jado (cadr exp))
          (printable-do (cadr exp)))
      (string-append "ja " (add-tone "do" 2)))) ;; (jado) case


;; Convert a whole canonic form plus a tone to a printable form

(define (cf->string cf tone)
  (cond ((symbol? cf) ;; c, 0, 1, 2, A, B, ...
         (symbol->string cf))

        ;; (do 1) and (jado 1)
        ((or (eq? (car cf) 'do) (eq? (car cf) 'jado))
         (printable-do-jado cf))
        
        ;; (li ((jado 1) (jado 2)) (...))
        ((eq? (car cf) 'li)
         (string-join
           (append (list (add-tone "li" tone))
                   (map printable-do-jado (cadr cf))
                   (list "bi")
                   (list (cf->string (caddr cf) 4)))
           " "))

        ;; (pred args...) or (lu to RU ... to)
        (#t
         (let ((stone (if (equal? (car cf) "lu") 4 5)))
              (string-join
                (append (list (add-tone (car cf) tone))
                        (map (lambda (a) (cf->string a stone))
                             (cdr cf))
                        (list "na"))
                " ")))))


;; Pretty-print a whole predicate
;; Example: <[c 0] (dủa A B)>

(define (remove-trailing-na str)
  (let ((len (string-length str)))
    (if (equal? "na" (substring str (- len 2)))
        (remove-trailing-na
         (string-trim-both (substring str 0 (- len 2))))
        str)))

(define (contains-fail? cf)
  (if (equal? 'fail cf)
      #t
      (if (pair? cf)
          (fold (lambda (a b) (or a b))
                (map contains-fail? cf))
          #f)))

(define (pred->string pred)
  (if (contains-fail? (gcf pred))
      "-"
      (format #f "<[~a] (~a)>"
              (string-join
                (map (lambda (type)
                       (if (number? type)
                           (number->string type)
                           (symbol->string type)))
                     (typelist pred))
                " ")
              (remove-trailing-na
                (cf->string (gcf pred) 4)))))


;; Perform "pretty-print" stage on interpretation output.

(define (stage-pretty-print interpret-output)
  (map pred->string interpret-output))