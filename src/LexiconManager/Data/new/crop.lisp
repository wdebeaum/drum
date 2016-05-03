;;;;
;;;; w::crop
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::crop
 (senses
  ((meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
   (LF-PARENT ONT::cut)
   (example "crop the image")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

(define-words :pos W::n 
 :words (
(w::crop
 (senses
  (
   (LF-PARENT ONT::plant)
   (TEMPL COUNT-PRED-TEMPL)
   (example "winter crop")
   )
  )
 )
))

