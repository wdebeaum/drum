;;;;
;;;; W::govern
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
 (W::govern
   (SENSES
    ((lf-parent ont::managing)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the device governs the flow of oxygen")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     )
    ))
))

