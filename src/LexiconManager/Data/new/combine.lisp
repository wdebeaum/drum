;;;;
;;;; W::combine
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::combine
   (wordfeats (W::morph (:forms (-vb) :nom W::combination)))
   (SENSES
    ((EXAMPLE "I combined the aspirin with my vitamins")
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    ((EXAMPLE "the aspirin combines with my vitamins")
     (LF-PARENT ONT::combine-objects)
     (TEMPL AFFECTED-AFFECTED1-XP-PP-TEMPL)
     )
    ;; need this template to go through the adjectival passive rule
    ((EXAMPLE "the combined costs")
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

