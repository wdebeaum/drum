;;;;
;;;; w::ferment
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::ferment
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::life-transformation)
   (example "ferment the cider")
   (templ agent-affected-xp-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::life-transformation)
   (example "the cider fermented")
   (templ affected-templ)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

