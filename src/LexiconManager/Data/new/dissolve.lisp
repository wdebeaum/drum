;;;;
;;;; W::dissolve
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::dissolve
    (SENSES
    ((LF-PARENT ONT::dissolve)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "The water dissolved the salt.")
     )
    ((LF-PARENT ONT::dissolve)
     (example "the salt dissolved.")
     (TEMPL affected-templ)
     )
     )
    )
))
