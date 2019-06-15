;;;;
;;;; w::cook
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::cook
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cook)
   (example "cook the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cook)
   (example "the turkey cooked all afternoon")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   (templ affected-templ)
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::create-by-cooking)
   (example "I like to cook")
   (templ agent-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   )
  ((meta-data :origin cause-result-relations :entry-date 20181015 :change-date nil :comments nil)
   (LF-PARENT ONT::create-by-cooking)
   (example "Cynthia cooked a mighty feast!")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
   )
  )
 )
))

