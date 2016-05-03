;;;;
;;;; W::dip
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::dip
   (wordfeats (W::morph (:forms (-vb) :nom w::dip)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date nil :comments nil :vn ("calibratable_cos-45.6-1" "funnel-9.3-1") :wn ("dip%2:30:01" "dip%2:35:00" "dip%2:34:00"))
     (EXAMPLE "the plane dipped")
     (LF-PARENT ONT::move-downward)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     )
    )
   )
))

