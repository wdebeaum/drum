;;;;
;;;; W::equal
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::equal
   (SENSES
    ((meta-data :origin calo :entry-date 20031222 :change-date 20090731 :wn ("equal%3:00:02") :comments html-purchasing-corpus)
      (LF-PARENT ONT::EQUAL)
     (SEM (F::GRADABILITY f::-))
     (example "are they equal to each other")
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE w::to))))
     )
     ((meta-data :origin calo :entry-date 20040112 :change-date 20090731 :wn ("equal%3:00:02") :comments html-purchasing-corpus)
      (LF-PARENT ONT::EQUAL)
      (SEM (F::GRADABILITY f::-))
      (example "are they equal in length")
      (templ adj-property-templ (xp (% w::pp (w::PTYPE w::in))))
     )
    )
   )
))

