;;;;
;;;; W::CIRCLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CIRCLE
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :wn ("circle%1:25:00") :comments nil)
     (example "take the circle with the heart on the side")
     (LF-PARENT ONT::SHAPE-OBJECT)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::circle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8"))
     (LF-PARENT ONT::surround)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cover,surround
     )
    )
   )
))

