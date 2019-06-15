;;;;
;;;; W::adjoin
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::adjoin
   (SENSES
    ((EXAMPLE "A and B adjoin" "A and B are adjoined")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1") :wn ("adjoin%2:35:03"))
     (LF-PARENT ONT::connected)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% w::pp (w::ptype (? p w::with w::to))))) ; like intersect
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cross,hit,meet
     )
    ((EXAMPLE "A is adjoined with B")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1") :wn ("adjoin%2:35:03"))
     (LF-PARENT ONT::connected)
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL) ; like hit,meet
     )
    )
   )
))

