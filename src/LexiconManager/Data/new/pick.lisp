;;;;
;;;; W::pick
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
((W::pick (w::out))
 (SENSES
    ((LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "pick out a computer")
     (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::pick
   (SENSES
    ((LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(PREFERENCE 0.95)
     (templ agent-neutral-xp-templ)
     (example "pick the route")
     )
    ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil)
     (LF-PARENT ONT::SELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "pick red")
     (TEMPL AGENT-FORMAL-TEMPL)
     ;(preference .95)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::pick (W::up))
   (SENSES
    ;;;; swier -- pick up the oranges.
    ((LF-PARENT ont::acquire)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (prototypical-word t)
     )
    #|
    ((LF-PARENT ont::clean)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin asma :entry-date 20111004)
     )
    |#
    ((LF-PARENT ONT::increase)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "use of wind energy picked up after the oil shortage")
     (meta-data :origin windmill :entry-date 20080604 :change-date 20090504 :comments nil)
     (templ affected-templ)
     )
    )
   )
))

