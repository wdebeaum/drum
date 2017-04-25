;;;;
;;;; W::urgent
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::urgent
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("urgent%5:00:00:imperative:00"))
     (lf-parent ont::urgent-val)
     (TEMPL adj-purpose-optional-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    )
   )
))

