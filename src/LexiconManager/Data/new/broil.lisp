;;;;
;;;; W::broil
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::broil
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("broil%2:30:00"))
     (LF-PARENT ONT::roast)
 ; like bake,blanch,boil,braise,cook,fry
     (EXAMPLE "broil the chicken wings")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

