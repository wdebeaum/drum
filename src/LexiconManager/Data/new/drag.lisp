;;;;
;;;; W::drag
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::drag
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("drag%2:35:02"))
     
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL agent-neutral-xp-templ) ; like check,search
     (example "drag the river")
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::haul)
     (example "drag the triangle")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcart-11-1 :vn ("carry-11.4") :wn ("drag%2:35:00" "drag%2:35:01"))
     )
    )
   )
))

