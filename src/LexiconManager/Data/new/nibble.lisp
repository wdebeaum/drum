;;;;
;;;; W::nibble
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::nibble
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he nibbled on bread")
     (SEM (F::ASPECT F::DYNAMIC))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL  (xp (% W::pp (W::ptype W::on))))
     )
     ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he nibbled the bread")
     (SEM (F::ASPECT F::DYNAMIC))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

