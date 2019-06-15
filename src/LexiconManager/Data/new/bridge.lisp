;;;;
;;;; W::BRIDGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BRIDGE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bridge%1:06:00"))
     (LF-PARENT ONT::bridge)
     (SEM (F::spatial-abstraction (? sabr F::spatial-point F::line)))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bridge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("bridge%2:42:00"))
     (LF-PARENT ONT::surround)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cover,surround
     )
    )
   )
))

