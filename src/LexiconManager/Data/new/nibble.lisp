;;;;
;;;; W::nibble
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::nibble
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he nibbled on bread")
     (SEM (F::ASPECT F::DYNAMIC))
     (templ agent-affected-xp-templ  (xp (% W::pp (W::ptype W::on))))
     )
     ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he nibbled the bread")
     (SEM (F::ASPECT F::DYNAMIC))
     (templ agent-affected-xp-templ)
     )
    )
   )
))

