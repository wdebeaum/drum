;;;;
;;;; W::CAPABLE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::CAPABLE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("capable%5:00:00:competent:00" "capable%3:00:00" "capable%5:00:00:adequate:00") :comments caloy2)
     (EXAMPLE "He is a capable person")
     (LF-PARENT ONT::able)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::of))))
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

