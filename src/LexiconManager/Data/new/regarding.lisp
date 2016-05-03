;;;;
;;;; w::regarding
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (w::regarding
   (SENSES
    ((LF-PARENT ONT::associated-information)
     (example "she would like to speak with you regarding the transportation situation")
     (TEMPL binary-constraint-s-or-np-templ)
     (meta-data :origin calo-ontology :entry-date 20060412 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::REGARDING)
   (SENSES
    ((LF-PARENT ONT::TOPIC)
     (LF-FORM W::TOPIC)
     (TEMPL TOPIC-TEMPL)
     )
    )
   )
))

