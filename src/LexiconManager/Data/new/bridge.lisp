;;;;
;;;; W::BRIDGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BRIDGE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bridge%1:06:00"))
     (LF-PARENT ONT::general-structure)
     (SEM (F::spatial-abstraction (? sabr F::spatial-point F::line)))
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::bridge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("bridge%2:42:00"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ) ; like cover,surround
     )
    )
   )
))

