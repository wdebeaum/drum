;;;;
;;;; W::arouse
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::arouse
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("arouse%2:29:00" "arouse%2:37:01"))
     (LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::from))))) ; like annoy,bother,concern,hurt
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "the proposal aroused anger")
     )
    )
   )
))

