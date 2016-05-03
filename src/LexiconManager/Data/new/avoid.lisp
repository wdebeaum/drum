;;;;
;;;; W::avoid
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::avoid
   (SENSES
    #||((LF-PARENT ONT::AVOID-LOCATION)
     (example "the truck avoided the lava")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL cause-theme-TEMPL)
     )||#
    ((lf-parent ont::avoid-location)
     (example "avoid the riot on 4th street")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-theme-xp-templ)
     (meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2 :vn ("avoid-52") :wn ("avoid%2:32:00" "avoid%2:34:00" "avoid%2:41:01" "avoid%2:41:03"))
     )
    ((lf-parent ont::avoiding)
     (example "avoid buying the computer if the price exceeds the budget")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-action-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
     (meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2)
     )
    )
   )
))

