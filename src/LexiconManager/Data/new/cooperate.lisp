;;;;
;;;; w::cooperate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (w::cooperate
   (wordfeats (W::morph (:forms (-vb) :nom w::cooperation)))
   (senses
    ((LF-parent ont::collaborate)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? p W::on W::with)))))
     )
    )
   )
))

