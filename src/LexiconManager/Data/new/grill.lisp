;;;;
;;;; W::grill
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::grill
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("grill%2:30:00"))
     (LF-PARENT ONT::roast) ; like bake,blanch,boil,braise,cook,fry
     (EXAMPLE "grill the chicken")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

