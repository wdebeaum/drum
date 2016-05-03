;;;;
;;;; w::launch
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::launch
    (wordfeats (W::morph (:forms (-vb) :nom w::launch)))
    (SENSES
     ((LF-PARENT ONT::START)
      (EXAMPLE "launch the browser")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050826 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ agent-affected-xp-templ)
      )
     ((LF-PARENT ONT::START)
      (EXAMPLE "launch the meeting")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050826 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (templ agent-effect-xp-templ)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

