;;;;
;;;; W::adjourn
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::adjourn
   (SENSES
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (EXAMPLE "let's adjourn the meeting")
     (meta-data :origin calo-ontology :entry-date 20060106 :change-date nil :comments meeting-understanding)
     )
    )
   )
))

