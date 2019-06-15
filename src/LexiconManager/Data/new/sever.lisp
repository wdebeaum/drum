;;;;
;;;; W::sever
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::sever
   (SENSES
    ((EXAMPLE "He severed communications")
     (LF-PARENT ONT::STOP)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo-ontology :entry-date 20050831 :change-date nil :comments Break-contact)
     )
    ((EXAMPLE "He severed the power lines")
     (LF-PARENT ONT::cut)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo-ontology :entry-date 20050831 :change-date nil :comments nil)
     )
    
    )
   )
))

