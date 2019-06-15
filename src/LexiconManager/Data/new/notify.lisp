;;;;
;;;; W::notify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::notify
   (wordfeats (W::morph (:forms (-vb) :nom w::notification)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "notify him that the plan changed")
     (LF-PARENT ONT::tell)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "They informed him of the decision")
     (LF-PARENT ONT::tell)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL (xp (% w::pp (w::ptype w::of)))) ; like notify
     )
    #|((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "notify him about the change")
     (LF-PARENT ONT::tell)
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
     )|#
    )
   )
))

