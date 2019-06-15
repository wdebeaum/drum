;;;;
;;;; W::govern
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
 (W::govern
   (SENSES
    ((lf-parent ont::governing);ont::managing
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the device governs the flow of oxygen")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     )
    ))
))

