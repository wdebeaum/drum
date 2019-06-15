;;;;
;;;; W::furnish
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::furnish
   (SENSES
    ((LF-PARENT ONT::supply)
     (example "furnish the office")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090501 :comments Office)
     )
    ((LF-PARENT ONT::supply)
     (example "the battery furnishes a spark (to the engine)")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-goal-optional-TEMPL)
     (meta-data :origin step :entry-date 20080626 :change-date 20090501 :comments nil)
     )
    ((LF-PARENT ONT::supply)
     (example "the battery furnishes the engine with power")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% w::pp (w::ptype w::with))))
     (meta-data :origin step :entry-date 20080626 :change-date 20090501 :comments nil)
     )
    )
   )
))

