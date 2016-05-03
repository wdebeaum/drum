;;;;
;;;; W::vape
;;;;

(define-words :pos W::n 
 :words (
   (W::vape
   (SENSES
    ((LF-PARENT ONT::DRUG)
     (example "He is using vape.")
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::vape
   (SENSES
    ((LF-PARENT ONT::Smoking)
     (example "He vaped electronic cigarettes." "He vaped in the house.")
     (TEMPL AGENT-AFFECTED-XP-optional-TEMPL)
     )
    )
   )
))
