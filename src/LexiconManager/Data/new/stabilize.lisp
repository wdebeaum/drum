;;;;
;;;; W::stabilize
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::flood
   (SENSES
    ((LF-PARENT ONT::stabilize)
     (example "the economy stabilized")
     (templ affected-templ)
     )
    ((LF-PARENT ONT::stabilize)
     (example "The news stabilized the economy")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

