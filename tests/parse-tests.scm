#!/usr/bin/guile
!#

(load "test-macro.scm")


(define parse-tests
  '(
    ;; Basic composition
    ("jai"               "jai")
    ("dua pai"           ("dua" "pai"))
    ("dua leo pai"       ("dua" ("leo" "pai")))
    ("kuai tua jeaq jai" ("kuai" ("tua" ("jeaq" "jai"))))
    
    ;; Mu
    ("mu cho"            (mu "cho"))
    ("leo mu mai"        ("leo" ("mu" "mai")))
    ("mu dua pai"        ((mu "dua") "pai"))
    ("kuai mu tua jai"   ("kuai" ((mu "tua") "jai")))

    ;; Ru
    ("pai ru mai"               (ru "pai" "mai"))
    ("niai ra poq dua"          ((ra "niai" "poq") "dua"))
    ("bu ru gi ro hui"          (ru "bu" (ro "gi" "hui")))
    ("jeo bu ru gi ro hui jeo"  ("jeo" ((ru "bu" (ro "gi" "hui")) "jeo")))
    ("mu pai re mai"            (ru (mu "pai") "mai"))
    ("pai re mu mai"            (ru "pai" (mu "mai")))
    ("mu pai ri mu mai"         (ri (mu "pai") (mu "mai")))

    ;; To ru
    ("to ru gi to hui"                  (ru "gi" "hui"))
    ("jeo to ra gi to hui"              ("jeo" (ra "gi" "hui")))
    ("jeo to re toa gu to toa jaq pui"  ("jeo" (re ("toa" "gu") ("toa" ("jaq" "pui")))))
    ("mu to ru gi to hui"               (mu (ru "gi" "hui")))
    
    ;; Multi-syllable predicates
    ))

(define parse-tests-as-tests
  (map (lambda (t) (list `(parse ,(car t))
                         `(quote ,(cadr t))))
       parse-tests))



;; Must (?) be defined as a macro so that the tests can be
;; passed to the macro without a variable name getting in the way
(define-macro (run-parse-tests display-anyway)
  `(run-tests ,parse-tests-as-tests
              display-anyway

              (fail-function "parse")
              (pass-function "parse")))