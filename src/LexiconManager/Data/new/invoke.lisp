;;;;
;;;; w::invoke
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::invoke
     (wordfeats (W::morph (:forms (-vb) :nom w::invocation)))
    (SENSES
     ((LF-PARENT ONT::boot-up)
      (EXAMPLE "invoke the applescript")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050831 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

