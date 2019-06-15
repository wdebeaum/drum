;;;;
;;;; W::assure
;;;;

(define-words :pos W::v 
 :words (
  (W::assure
     (wordfeats (W::morph (:forms (-vb) :nom W::assurance)))
   (SENSES
    #|((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::assert)
     (example "he assured me that they sang")
     (TEMPL agent-addressee-theme-optional-templ) ; like inform
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::assert)
     (TEMPL agent-addressee-associated-information-templ) ; like notify
     )|#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::assert)
     (example "he assured me that they sang")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like notify
     )
    )
   )
))

