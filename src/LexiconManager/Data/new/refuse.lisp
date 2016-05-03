;;;;
;;;; W::refuse
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::refuse
   (senses
    ((lf-parent ont::refuse)
     (templ agent-affected-optional-templ)
     (example "he refused the package")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )      
    ((lf-parent ont::refuse)
      (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (example "he refused to accept the charges")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )  
    )
  )
))

