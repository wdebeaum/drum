;;;;
;;;; W::tend
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::tend
   (SENSES
    ((LF-PARENT ONT::be-inclined)
     (example "It tends to increase.")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     )
        )
   )
))

