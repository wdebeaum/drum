;;;;
;;;; W::commence
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::commence
     (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (lf-parent ont::start)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he commenced to eat")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (lf-parent ont::start)
     (example "the managers commenced working")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::vp (w::vform w::ing))))
     )
#|    (
     (lf-parent ont::start)
     (example "the computers commenced working")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-theme-subjcontrol-templ (xp (% w::vp (w::vform w::ing))))
      )
|#
     ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (lf-parent ont::start)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the computers commenced to work")
     (TEMPL AFFECTED-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to))))
     )
     ((LF-PARENT ONT::START)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "commence the festivities")
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::START)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL affected-TEMPL)
     (example "the party commenced")
     )
    ((LF-PARENT ONT::START)
     (meta-data :origin fruit-carts :entry-date 20050331 :change-date nil :comments fruitcarts-11-3)
     (example "commence with the triangle")
     (SEM (F::cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL (xp (% W::pp (W::ptype W::with))))
     )
    )
   )
))

