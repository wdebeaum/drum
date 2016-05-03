;;;;
;;;; w::assemble
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::assemble
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("herd-47.5.2") :wn ("assemble%2:41:00" "assemble%2:41:03"))
     (LF-PARENT ONT::meet)
     (example "they assembled at the church")
     (TEMPL agent-plural-templ)
     )

    ((LF-PARENT ONT::joining)
     (example "assemble the parts")
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date 20090505 :comments caloy3)
     (TEMPL agent-affected-PLURAL-TEMPL) 
     )

    ((LF-PARENT ONT::joining)
     (example "the pieces assemble (into a toy)")
     (TEMPL affected-result-optional-templ) 
     )

    )
   )
))

