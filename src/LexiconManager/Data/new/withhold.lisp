;;;;
;;;; W::withhold
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::withhold
    (wordfeats (W::morph (:forms (-vb) :ing W::withholding :past W::withheld)))
   (senses
    ((lf-parent ont::refuse)
     (templ agent-affected-xp-templ)
     (example "he withheld evidence at the trial")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )
    )
   )
))

