;;;;
;;;; W::circumvent
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::circumvent
   (SENSES
    ((LF-PARENT ONT::AVOID-LOCATION)
     (example "the caravan circumvented the lava")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2)
     (TEMPL AGENT-theme-xp-TEMPL)
     )
    ((lf-parent ont::avoiding)
     (example "how did you circumvent the problem")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-neutral-xp-templ)
     (meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2 :vn ("avoid-52"))
     )
    )
   )
))

