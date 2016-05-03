;;;;
;;;; w::cook
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::cook
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "cook the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "I like to cook")
   (templ agent-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "the turkey cooked all afternoon")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   (templ affected-templ)
   )
  )
 )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::cook w::up)
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("preparing-26.3-1"))
     (LF-PARENT ONT::cooking)
     )
    )
   )
))

