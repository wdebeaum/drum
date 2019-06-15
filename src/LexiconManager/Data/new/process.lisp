;;;;
;;;; w::process
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::process
   (senses
    ((lf-parent ont::process)
     (templ other-reln-templ)
     (example "the process for solving this problem")
     (meta-data :origin calo :entry-date 20040622 :change-date 20050817 :comments lf-restructuring :wn ("process%1:03:00"))
     )	   	   	   	   
    ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::process
   (SENSES
    ((LF-PARENT ONT::nature-change)
     (example "process the oranges into juice")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-RESULT-XP-PP-INTO-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::into))))
     )
    )
   )
))

