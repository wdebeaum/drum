;;;;
;;;; w::frost
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::frost
 (senses
  ((meta-data :origin foodkb :entry-date 20090129 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (syntax (w::resultative +))
   (example "he frosted the cake")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

