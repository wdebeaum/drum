;;;;
;;;; W::redundant
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::redundant
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20041116 :change-date 20090915 :wn ("redundant%5:00:00:unnecessary:00") :comments caloy2)
     (LF-PARENT ONT::part-whole-val)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::with))))
     )
    )
   )
))

