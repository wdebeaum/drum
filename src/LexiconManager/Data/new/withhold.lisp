;;;;
;;;; W::withhold
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::withhold
    (wordfeats (W::morph (:forms (-vb) :ing W::withholding :past W::withheld)))
   (senses
    ((lf-parent ont::refuse)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he withheld evidence at the trial")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )
    )
   )
))

