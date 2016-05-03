;;;;
;;;; W::dare
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::dare
   (SENSES
    ((LF-PARENT ONT::PROVOKE)
     (TEMPL agent-effect-subjcontrol-templ)
     (example "he dared to release the storm report")
     )
    ((LF-PARENT ONT::PROVOKE)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)
     (example "he dared him to release the storm report")
     )
    )
   )
))

