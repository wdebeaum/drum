;;;;
;;;; w::invoke
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::invoke
     (wordfeats (W::morph (:forms (-vb) :nom w::invocation)))
    (SENSES
     ((LF-PARENT ONT::START)
      (EXAMPLE "invoke the applescript")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050831 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (templ agent-effect-xp-templ)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

