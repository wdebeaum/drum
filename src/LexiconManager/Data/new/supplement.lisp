;;;;
;;;; W::supplement
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::supplement
   (wordfeats (W::morph (:forms (-vb) :nom W::supplement)))
   (SENSES
    ((EXAMPLE "I supplemented my diet with vitamins")
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-AFFECTED-TEMPL)
     )
    ;; need this template to go through the adjectival passive rule
     ((LF-PARENT ONT::add-include)
     (example "the supplemented diet")
     (TEMPL AGENT-RESULT-XP-NP-TEMPL)
     )
    )
   )
))

