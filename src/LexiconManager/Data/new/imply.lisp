;;;;
;;;; W::imply
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::imply
   (wordfeats (W::morph (:forms (-vb) :nom W::implication)))
   (SENSES
    ((LF-PARENT ONT::correlation) ; like indicate
     (example "a cough implies a cold")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("indicate-78"))
     )
    ((LF-PARENT ONT::correlation) ; like indicate
     (example "This implies that the cat ate the mouse.")
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    )
   )
))

