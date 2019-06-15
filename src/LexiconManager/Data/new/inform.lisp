;;;;
;;;; W::inform
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::inform
   (SENSES
    #|((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::tell)
     (TEMPL agent-addressee-associated-information-templ) ; like notify
     (PREFERENCE 0.96)
    )|#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "They informed him of the decision")
     (LF-PARENT ONT::tell)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL (xp (% w::pp (w::ptype w::of)))) ; like notify
     )
    
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::tell)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like notify
     (PREFERENCE 0.96)
     )
    )
  )
))

