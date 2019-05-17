;;;;
;;;; W::inspire
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::inspire
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("inspire%2:32:00"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like annoy,bother,concern,hurt
     (example "Napoleon inspired him [to become the Emporer's painter]")
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::among))))) ; like annoy,bother,concern,hurt
     (templ agent-affected-xp-templ)
     (example "he inspired confidence [in them]")
     )
    )
   )
))

