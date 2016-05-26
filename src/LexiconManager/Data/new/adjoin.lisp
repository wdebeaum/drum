;;;;
;;;; W::adjoin
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::adjoin
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-neutral-xp-templ (xp (% w::pp (w::ptype w::with)))) ; like intersect
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-neutral-xp-templ) ; like cross,hit,meet
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::connected)
     (TEMPL neutral-plural-templ) ; like hit,meet
     )
    )
   )
))

