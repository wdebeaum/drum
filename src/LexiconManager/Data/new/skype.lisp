;;;;
;;;; W::skype
;;;;

(define-words :pos W::n 
 :words (
  (W::skype
   (SENSES
    ((LF-PARENT ONT::computer-program)
     (TEMPL nname-templ)
     (example "I use skype.")
     )
    )
   )
))


(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::skype
   (SENSES
     (
     (LF-PARENT ONT::ESTABLISH-COMMUNICATION)
     (example "I skyped him.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
     (
     (LF-PARENT ONT::TALK)
     (example "I skyped with him about this.")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp1 (% w::pp (w::ptype (? ptp w::with)))))
     )
    )
   )
))
