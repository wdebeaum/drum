;;;;
;;;; W::supplement
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::supplement
   (wordfeats (W::morph (:forms (-vb) :nom W::supplement)))
   (SENSES
    ((EXAMPLE "I supplemented my diet with vitamins")
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-GOAL-THEME-OPTIONAL-TEMPL)
     )
    ;; need this template to go through the adjectival passive rule
     ((LF-PARENT ONT::add-include)
     (example "the supplemented diet")
     (TEMPL AGENT-GOAL-xp-TEMPL)
     )
    )
   )
))

