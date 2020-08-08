;;;;
;;;; W::unequal
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::unequal
   (SENSES
    ((meta-data :origin calo :entry-date 20031222 :change-date nil :wn ("unequal%3:00:02") :comments html-purchasing-corpus)
      (LF-PARENT ONT::identity-val)
     (SEM (F::GRADABILITY -))
     (example "are they unequal to each other")
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE w::to))))
     )
     ((meta-data :origin calo :entry-date 20040112 :change-date nil :wn ("unequal%3:00:02") :comments html-purchasing-corpus)
      (LF-PARENT ONT::identity-val)
      (SEM (F::GRADABILITY -))
      (example "are they unequal in length")
      (templ adj-property-templ (xp (% w::pp (w::PTYPE w::in))))
     )
    )
   )
))

