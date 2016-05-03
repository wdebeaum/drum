;;;;
;;;; W::NEW
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::NEW
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("new%5:00:00:original:00" "new%5:00:00:other:00"))
     (LF-PARENT ONT::novelty-VAL)
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((w::new w::year^s w::day)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

(define-words :pos W::name
 :words (
  ((w::new w::year^s w::eve)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

