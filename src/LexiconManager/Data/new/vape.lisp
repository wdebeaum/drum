;;;;
;;;; W::vape
;;;;

(define-words :pos W::n 
 :words (
   (W::vape
   (SENSES
    ((LF-PARENT ONT::device)
     (example "He is using a vape.")
     (templ count-pred-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::vape
   (SENSES
    ((LF-PARENT ONT::Smoking)
     (example "He vaped electronic cigarettes." "He vaped in the house.")
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-A-TEMPL)
     )
    )
   )
))
