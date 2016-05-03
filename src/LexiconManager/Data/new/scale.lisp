;;;;
;;;; W::SCALE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SCALE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::measure-domain)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
;; Various measuring devices
  (w::scale
  (senses((LF-parent ONT::Device) 
	    (templ count-pred-templ)
	    (meta-data :origin bee :entry-date 20040407 :change-date 20040621 :comments (test-s portability-followup))
	    ))
)
))

(define-words :pos W::v 
 :words (
((w::scale (w::down))
 (senses
  ((meta-data :origin task-learning :entry-date 20050829 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::decrease)
   (example "scale down the image resolution")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

