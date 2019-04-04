;;;;
;;;; W::adjoin
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::adjoin
   (SENSES
    ((EXAMPLE "A and B adjoin" "A and B are adjoined")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1") :wn ("adjoin%2:35:03"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-neutral-xp-templ (xp (% w::pp (w::ptype (? p w::with w::to))))) ; like intersect
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-neutral-xp-templ) ; like cross,hit,meet
     )
    ((EXAMPLE "A is adjoined with B")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1") :wn ("adjoin%2:35:03"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-plural-templ) ; like hit,meet
     )
    )
   )
))

