;;;;
;;;; w::revert
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (w::revert
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060609 :change-date nil :comments nil :vn ("convert-26.6.2"))
     ;(LF-PARENT ONT::adjust)
     (LF-PARENT ONT::undo)
     (example "revert the document to the saved version")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-RESULT-XP-PP-INTO-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt W::to)))))
     )
    )
   )
))

