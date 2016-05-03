;;;;
;;;; W::collaborate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::collaborate
    (wordfeats (W::morph (:forms (-vb) :nom w::collaboration)))
   (SENSES
    ((LF-PARENT ONT::collaborate)
     (example "they collaborated on the plan")
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::PP (W::ptype (? p W::on W::with)))))
     )
    )
   )
))

