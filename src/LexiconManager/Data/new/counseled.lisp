;;;;
;;;; W::counseled
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::counseled
   (wordfeats (W::morph (:forms NIL)) (W::vform (? vf W::past w::pastpart)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL) ; like warn
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     )
    )
   )
))

