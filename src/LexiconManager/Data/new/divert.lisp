;;;;
;;;; W::divert
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::divert
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1"))
     (LF-PARENT ONT::evoke-amusement)
     (example "diverted himself by picking up shells on the sea-shore.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (PREFERENCE 0.96)
     )
    ;;;; Divert the cargo
    ((LF-PARENT ONT::divert)
     (Example "They diverted the cargo.")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

