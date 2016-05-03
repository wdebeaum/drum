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


(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::skype
   (SENSES
     (
     (LF-PARENT ONT::ESTABLISH-COMMUNICATION)
     (example "I skyped him.")
     (TEMPL AGENT-affected-XP-TEMPL)
     )
     (
     (LF-PARENT ONT::TALK)
     (example "I skyped with him about this.")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp1 (% w::pp (w::ptype (? ptp w::with)))))
     )
    )
   )
))
