;;;;
;;;; w::launch
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::launch
    (wordfeats (W::morph (:forms (-vb) :nom w::launch)))
    (SENSES
     ((LF-PARENT ONT::START)
      (EXAMPLE "launch the browser")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050826 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     ((LF-PARENT ONT::START)
      (EXAMPLE "launch the meeting")
      (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050826 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

