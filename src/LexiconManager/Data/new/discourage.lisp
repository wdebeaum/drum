;;;;
;;;; W::discourage
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::discourage
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
 ; like block
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It discourages him from doing something")
     )
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("discourage%2:37:00"))
     (LF-PARENT ONT::evoke-sadness)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

