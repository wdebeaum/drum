;;;;
;;;; w::pasturize
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::pasturize ;; common misspelling
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::nature-change)
   (example "pasturize the milk")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

