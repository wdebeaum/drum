;;;;
;;;; w::initiate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::initiate
    (wordfeats (W::morph (:forms (-vb) :nom w::initiation)))
    (SENSES
     ((LF-PARENT ONT::START)
      (EXAMPLE "initiate the transition")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050830 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
)))

