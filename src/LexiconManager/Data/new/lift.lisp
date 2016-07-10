;;;;
;;;; w::lift
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::lift
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("lift%1:06:00") :comments Office)
     (LF-PARENT ONT::device)
     (example "take the lift to the fifth floor")
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::lift
   (wordfeats (W::morph (:forms (-vb) :nom w::lift)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_direction-9.4") :wn ("lift%2:35:00" "lift%2:35:01" "lift%2:38:00"))
;     (LF-PARENT ONT::lift)
     (LF-PARENT ONT::move-upward)
 ; like raise,lower
     )
    ((example "wait until the fog lifts")
    (lf-parent ont::disperse)
    )
  )
 )
))

