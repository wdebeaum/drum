;;;;
;;;; w::bake
;;;;

(define-words :pos W::V 
  :templ agent-affected-create-templ
 :words (
(w::bake
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::create-by-cooking)
   (example "bake the cake")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::create-by-cooking)
   (example "I like to bake")
   (templ agent-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::create-by-cooking)
   (example "the bread baked in the oven")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (f::time-span f::extended))
   (templ affected-templ)
   )
  ((meta-data :origin cause-result-relations :entry-date 20181010 :change-date nil :comments nil)
   (LF-PARENT ONT::bake)
   (example "She baked the turkey in the oven.")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (f::time-span f::extended))
   (templ agent-affected-xp-templ)
   )
  )
 )
))

