;;;;
;;;; W::deny
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::deny
    (wordfeats (W::morph (:forms (-vb) :nom W::denial)))
   (senses
    ((lf-parent ont::refuse)
     (templ agent-affected-xp-templ)
     (example "he denied the charges")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     )
    (;;(lf-parent ont::contest)
     (lf-parent ont::contest-deny-oppose-protest) ;; 20120523 GUM change new parent
     (TEMPL agent-theme-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-that)))))
     (example "he denied that he was in town the night of the murder")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090508 :comments caloy3)
     )
    )
   )
))

