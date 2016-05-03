;;;;
;;;; W::combine
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::combine
   (wordfeats (W::morph (:forms (-vb) :nom W::combination)))
   (SENSES
    ((EXAMPLE "I combined the aspirin with my vitamins")
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-affected2-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    ;; need this template to go through the adjectival passive rule
    ((EXAMPLE "the combined costs")
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-affected-xp-templ)
     )
    )
   )
))

