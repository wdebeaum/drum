;;;;
;;;; W::touch
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::touch
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("touch%2:37:00"))
     (LF-PARENT ONT::evoke-attraction)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cross,hit,meet
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::touch)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like cross,hit,meet
     )
    
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::touch)
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::touch
  (senses
   ((LF-PARENT ONT::ability-to-touch)
    (TEMPL MASS-PRED-TEMPL)
    )
   )
)
))
