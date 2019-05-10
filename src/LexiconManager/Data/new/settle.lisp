;;;
;;;; W::SETTLE
;;;;

(define-words :pos W::v 
 :words (
  (W::SETTLE
   (SENSES
    ((LF-PARENT ONT::position)
     (example "the immigrants settled in the west" "the dust settled on the counter")
     (TEMPL agent-TEMPL)
     (meta-data :origin step :entry-date 20080623 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::decide)
     (example "they settled the issue")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-theme-xp-templ)
     (meta-data :origin general :entry-date 20110131 :change-date nil :comments jansen)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((W::settle (w::down))
   (SENSES
    ((LF-PARENT ont::evoke-relief)
     (example "settle down")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL affected-TEMPL)
     ;(preference .97) ;; prefer transitive
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::evoke-relief)
     (example "settle him down")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    )
   )
))

