;;;;
;;;; W::refuse
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::refuse
   (senses
    ((lf-parent ont::refuse)
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL)
     (example "he refused the package")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )      
    ((lf-parent ont::refuse)
      (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (example "he refused to accept the charges")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )  
    )
  )
))

