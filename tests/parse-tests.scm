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
    ("to ri gi ro jaq hui to tuao bu"   (ri ((ro "gi" "jaq") "hui") ("tuao" "bu")))

    ;; Giants
    
    ;;         (     [         ]    [        ])      (     )
    ("mu to ru to ru tuao mu dua to jaq mu chi to mu hao hao"
     (mu (ru (ru ("tuao" (mu "dua")) ("jaq" (mu "chi"))) ("hao" (mu "hao")))))

    ;;                             RI (  RU [            RA <                 >    <            >]   [            ])   ( )
    ("mu mu jaq mu chi mu mu mu to ri to ru mu rai mu to ra dua leo ru buaq bai to mu loaq ro bie to chie ri mu tuq to rai"
     
     ((mu (mu "jaq"))
      ((mu "chi")
       (mu (mu (mu (ri (ru ((mu "rai")
                            (mu (ra ("dua" ((ru "leo" "buaq") "bai")) (ro (mu "loaq") "bie"))))
                           (ri "chie" (mu "tuq")))
                       "rai")))))))
     
    ;; Multi-syllable predicates
    ("maomao"                "maomao")
    ("maomao ra poq dua"     ((ra "maomao" "poq") "dua"))
    ("jaqbuaitoalalala rai"  ("jaqbuaitoalalala rai"))

    ;; Formatting
    ("kui            rai"       ("kui" "rai"))
    ("     soq     heqshea   "  ("soq" "heqshea"))

    ;; Invalid serials (not yet implemented)
    
    ))

(define parse-tests-as-tests
  (map (lambda (t) (list `(parse ,(car t))
                         `(quote ,(cadr t))))
       parse-tests))



;; Must (?) be defined as a macro so that the tests can be
;; passed to the macro without a variable name getting in the way
(define-macro (run-parse-tests display-anyway)
  `(run-tests ,parse-tests-as-tests
              ,display-anyway

              (fail-function "parse")
              (pass-function "parse")))