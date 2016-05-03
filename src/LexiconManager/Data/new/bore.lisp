;;;;
;;;; W::bore
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::bore
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("bore%2:37:00"))
     (LF-PARENT ONT::evoke-boredom)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("carve-21.2-2"))
     (LF-PARENT ONT::cut)
     (example "the machine bored the metal")
 ; like chop
     )
    )
   )
))

