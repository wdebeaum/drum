;;;;
;;;; W::kill
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::kill
   (SENSES
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "kill the program")
     (meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comments caloy3)
     )
    ((meta-data :origin step :entry-date 20080705 :change-date nil :comments nil)
     (LF-PARENT ONT::kill)
     )
    ((meta-data :origin step :entry-date 20080705 :change-date nil :comments nil)
     (LF-PARENT ONT::kill)
     (TEMPL agent-templ)
     (example "Poison kills.")
     )
   
    )
   )
))

