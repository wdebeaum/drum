;;;;
;;;; W::indicate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::indicate
   (wordfeats (W::morph (:forms (-vb) :nom W::indication)))
   (SENSES
    ((LF-PARENT ONT::correlation)
     (example "a cough indicates a cold")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    ((LF-PARENT ONT::correlation)
     (example "a cough indicates whether a person has a cold")
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::cp (w::ctype (? ct w::s-finite)))))
     )
    )
   )
))

