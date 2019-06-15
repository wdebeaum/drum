;;;;
;;;; W::swear
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::swear
   (wordfeats (W::morph (:forms (-vb) :past W::swore :pastpart W::sworn)))
   (SENSES
    ((LF-PARENT ONT::manner-say)
     (example "swear that it's true")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::manner-say)
     (example "swear to you that it's true")
     (TEMPL AGENT-RESULT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((lf-parent ont::swear)  ;; 20120523 GUM change new parent
     (TEMPL agent-templ)
     (preference .98)
     (EXAMPLE "he swore (at him)")
     (meta-data :origin cardiac :entry-date 20090121 :change-date nil :comments nil)
     )
    )
   )
))

