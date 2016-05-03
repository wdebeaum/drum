;;;;
;;;; W::equivalent
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::equivalent
   (SENSES
    ((meta-data :origin calo :entry-date 20031205 :change-date 20090731 :wn ("equivalent%3:00:04") :comments calo-y1script)
     (LF-PARENT ONT::EQUAL)
     (SEM (F::GRADABILITY f::-))
     (example "are they equivalent to each other")
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE w::to))))
     )
    ((meta-data :origin calo :entry-date 20040112 :change-date 20090731 :wn ("equivalent%3:00:04") :comments calo-y1script)
     (LF-PARENT ONT::EQUAL)
     (SEM (F::GRADABILITY f::-))
     (example "are they equivalent in speed")
     (templ adj-property-templ (xp (% w::pp (w::PTYPE w::in))))
     )
    )
   )
))

