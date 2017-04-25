;;;;
;;;; W::SPECIFIC
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SPECIFIC
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("specific%3:00:00"))
     (lf-parent ont::specific-val)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::to))))
     )
    )
   )
))

