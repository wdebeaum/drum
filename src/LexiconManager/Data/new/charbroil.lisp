;;;;
;;;; W::charbroil
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::charbroil
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::roast)
     (EXAMPLE "John charbroiled the chicken")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

