;;;;
;;;; W::healthy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::healthy
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (LF-PARENT ONT::body-PROPERTY-VAL)
     (example "healthy person")
     (templ central-adj-experiencer-templ)
     )
     ((meta-data :origin calo :entry-date 20050527 :change-date nil :wn ("curious%3:00:00") :comments projector-purchasing)
     (LF-PARENT ONT::body-property-val)
     (example "a healthy diet" "a healthy lifestyle")
     (templ central-adj-content-templ)
     )
    )
   )
))

