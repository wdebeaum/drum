;;;;
;;;; W::reconnect
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reconnect
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (meta-data :origin calo-ontology :entry-date 20060130 :change-date nil :comments meeting-understanding)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
    )
    #||((LF-PARENT ONT::ATTACH)
     (meta-data :origin calo-ontology :entry-date 20060130 :change-date nil :comments meeting-understanding)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     )||#
    )
   )
))

