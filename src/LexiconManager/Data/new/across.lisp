;;;;
;;;; W::ACROSS
;;;;

(define-words :pos W::ADV
 :words (
  (W::ACROSS
   (SENSES
    #|((LF-PARENT ONT::TO-LOC)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )||#
    ((LF-PARENT ONT::pos-as-opposite)
     (example "the house across the street")
     (meta-data :origin calo-ontology :entry-date 20060423 :change-date nil :comments addresses)
     ;(TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )

    ((LF-PARENT ONT::ACROSS)
     ;(TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    
    )
   )
))

