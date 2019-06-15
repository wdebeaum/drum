;;;;
;;;; W::reflect
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::reflect
   (wordfeats (W::morph (:forms (-vb) :nom W::reflection)))
   (SENSES
    ((LF-PARENT ONT::encodes-message)
     (example "his actions reflect his beliefs")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    ((LF-PARENT ONT::bounce-reflect)
     (example "sound is reflected well in this auditorium.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::bounce-reflect)
     (example "The light reflected.")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

