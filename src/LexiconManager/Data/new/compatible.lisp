;;;;
;;;; W::compatible
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::compatible
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20041116 :change-date 20090818 :wn ("compatible%3:00:01") :comments caloy2)
     (lf-parent ont::compatible-val)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::with))))
     )
    )
   )
))

