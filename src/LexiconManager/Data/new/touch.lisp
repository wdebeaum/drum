;;;;
;;;; W::touch
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::touch
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("touch%2:37:00"))
     (LF-PARENT ONT::evoke-attraction)
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::touch)
     (TEMPL neutral-neutral-xp-templ) ; like cross,hit,meet
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::touch)
     (TEMPL agent-affected-xp-templ) ; like cross,hit,meet
     )
    
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("contiguous_location-47.8-1"))
     (LF-PARENT ONT::touch)
     (TEMPL neutral-plural-templ)
     )
    )
   )
))

