;;;;
;;;; W::inspire
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::inspire
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("inspire%2:32:00"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)  ; like annoy,bother,concern,hurt
     (example "Napoleon inspired him [to become the Emporer's painter]")
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::among))))) ; like annoy,bother,concern,hurt
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he inspired confidence [in them]")
     )
    )
   )
))

