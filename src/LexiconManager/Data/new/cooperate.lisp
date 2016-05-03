;;;;
;;;; w::cooperate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::cooperate
   (wordfeats (W::morph (:forms (-vb) :nom w::cooperation)))
   (senses
    ((LF-parent ont::collaborate)
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::PP (W::ptype (? p W::on W::with)))))
     )
    )
   )
))

