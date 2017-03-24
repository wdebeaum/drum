;;;;
;;;; w::pasteurize
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::pasteurize
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   ;(LF-PARENT ONT::nature-change)
   (LF-PARENT ONT::transform-to-preserve)
   (example "pasturize the milk")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

