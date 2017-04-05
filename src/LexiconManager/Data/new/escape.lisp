;;;;
;;;; W::escape
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
 (W::escape
   (SENSES
    ((lf-parent ont::avoiding)
     (example "you cannot escape the consequences")
     (meta-data :origin calo :entry-date 20041201 :change-date 20090608 :comments caloy2)
     )
    ((lf-parent ont::depart)
     (example "he escaped from the party")
     (templ agent-source-templ (xp (% W::PP (W::ptype W::from))))
     (meta-data :origin calo :entry-date 20041201 :change-date 20090608 :comments caloy2)
     )
    )
   )
))

