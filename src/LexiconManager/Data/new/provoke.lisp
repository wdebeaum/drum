;;;;
;;;; W::provoke
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::provoke
    (wordfeats (W::morph (:forms (-vb) :nom W::provocation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("provoke%2:37:00" "provoke%2:37:01"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He provoked him [to run for office]")     
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL  agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::from))))) ; like annoy,bother,concern,hurt
     (templ agent-affected-xp-templ)
     (example "he provoked a response [in the audience]")
     )
    ; he provoked him into running for office
    )
   )
))

