;;;;
;;;; W::escape
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
 (W::escape
   (SENSES
    ((lf-parent ont::escape)
     (example "you cannot escape the consequences")
     ;;(templ agent-source-templ)
     (meta-data :origin calo :entry-date 20041201 :change-date 20090608 :comments caloy2)
     )
    ((lf-parent ont::escape)
     (example "he escaped from the party")
     (templ agent-source-templ (xp (% W::PP (W::ptype W::from))))
     (meta-data :origin calo :entry-date 20041201 :change-date 20090608 :comments caloy2)
     )
    )
   )
))

