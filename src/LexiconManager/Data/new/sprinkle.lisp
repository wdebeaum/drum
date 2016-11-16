;;;;
;;;; W::sprinkle
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::sprinkle
   (wordfeats (W::morph (:forms (-vb) :nom w::sprinkle)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::push-liquid)
 ; like spray
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("sprinkle%2:43:00"))
     (LF-PARENT ONT::precipitating)
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

