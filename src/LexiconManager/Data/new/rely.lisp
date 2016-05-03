;;;;
;;;; W::rely
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::rely
   (SENSES
    ((LF-PARENT ont::rely)
     (TEMPL agent-neutral-XP-TEMPL (xp (% W::PP (W::ptype W::on))))
     (example "they rely on them")
     )
    )
   )
))

