;;;;
;;;; w::sweeten
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
;; verbs for foodkb
(w::sweeten
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   ;(LF-PARENT ONT::increase)
   (LF-PARENT ONT::alter-flavor)
   (example "sweeten the cream")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

