;;;;
;;;; W::glide
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::glide
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("glide%2:38:00" "glide%2:38:02"))
     (LF-PARENT ONT::move-fluidly)
     (TEMPL affected-templ) ; like move,bounce
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("glide%2:38:00" "glide%2:38:02"))
     (LF-PARENT ONT::move-fluidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

