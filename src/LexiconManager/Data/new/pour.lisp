;;;;
;;;; W::pour
;;;;

(define-words :pos W::v 
 :words (
  (W::pour
   (SENSES
    ((meta-data :origin "kitchen" :entry-date 20101118 :change-date nil :comments nil :wn ("pour%2:38:00"))
     ;(LF-PARENT ont::pour)
     (LF-PARENT ont::fluidic-motion)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he poured the water over the floor")
     )
    ((meta-data :origin "kitchen" :entry-date 20101118 :change-date nil :comments nil :wn ("pour%2:38:00"))
;     (LF-PARENT ont::pour)
     (LF-PARENT ont::fluidic-motion)
     (templ affected-templ)
     (example "the water poured over the floor")
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("pour%2:43:00"))
     (LF-PARENT ont::precipitating)
     (example "when it rains it pours")
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

