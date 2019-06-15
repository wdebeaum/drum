;;;;
;;;; w::frost
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::frost
 (senses
  ((meta-data :origin foodkb :entry-date 20090129 :change-date nil :comments nil)
   (LF-PARENT ONT::coat-food)
   (syntax (w::resultative +))
   (example "he frosted the cake")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  )
 )
))

