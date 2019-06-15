;;;;
;;;; W::dare
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dare
   (SENSES
    ((LF-PARENT ONT::PROVOKE)
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL)
     (example "he dared to release the storm report")
     )
    ((LF-PARENT ONT::PROVOKE)
     (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)
     (example "he dared him to release the storm report")
     )
    )
   )
))

