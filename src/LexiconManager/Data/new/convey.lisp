;;;;
;;;; W::convey
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::convey
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::transport)
     (example "convey the cargo to avon")
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090529 :comments Convey)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    
   ((LF-PARENT ONT::REPEAT)
    (example "please convey my greetings to him")
     (TEMPL AGENT-FORMAL-AFFECTED-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090506 :comments Convey)
     )
    )
   )
))

