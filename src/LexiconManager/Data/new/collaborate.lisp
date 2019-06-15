;;;;
;;;; W::collaborate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::collaborate
    (wordfeats (W::morph (:forms (-vb) :nom w::collaboration)))
   (SENSES
    ((LF-PARENT ONT::collaborate)
     (example "they collaborated on the plan")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? p W::on W::with)))))
     )
    )
   )
))

