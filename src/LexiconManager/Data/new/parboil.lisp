;;;;
;;;; W::parboil
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::parboil
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("parboil%2:30:00"))
     (LF-PARENT ONT::boil)
     (EXAMPLE "parboil the noodles")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended)) ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

