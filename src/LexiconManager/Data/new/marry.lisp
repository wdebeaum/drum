;;;;
;;;; W::marry
;;;;

(define-words :pos W::v 
 :words (
  (W::marry
   (wordfeats (W::morph (:forms (-vb) :nom w::marriage :nomobjpreps (w::between w::to w::of))))
   (SENSES
    ((LF-PARENT ONT::MARRY)
     (TEMPL AGENT-AGENT1-XP-NP-TEMPL (xp (% W::NP (W::agr (? a W::1s W::2s W::3s)))))
     (EXAMPLE "Elizabeth married Christopher.")
     )

    ((LF-PARENT ONT::MARRY)
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (EXAMPLE "Elizabeth and Christopher married")
     )

    ((LF-PARENT ONT::MARRY)
     (preference .98)
     (TEMPL agent-affected-PLURAL-TEMPL)
     (EXAMPLE "Johnathan married Elizabeth and Christopher")
     )

    ((LF-PARENT ONT::MARRY)
     (preference .98)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to)))))
     (EXAMPLE "Johnathan married Elizabeth to Christopher")
     )

    )
   )
))

