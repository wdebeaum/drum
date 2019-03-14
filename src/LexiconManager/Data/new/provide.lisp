;;;;
;;;; W::provide
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::provide
      (wordfeats (W::morph (:forms (-vb) :nom W::provision)))
   (SENSES
    ((LF-PARENT ONT::supply)
     (example "provide the food (to him)")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     ;(TEMPL agent-affected-xp-TEMPL)
     (templ AGENT-AFFECTED-AR-TO-optional-TEMPL (xp (% w::pp (w::ptype w::to))))
     )
    ((LF-PARENT ONT::supply)
     (example "provide him with the food")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-recipient-affected-templ (xp (% w::pp (w::ptype w::with))))
     (meta-data :origin calo-ontology :entry-date 20050922 :change-date 20090501 :comments vn2-integration)
     )
    ((LF-PARENT ONT::enable)
     (example "provide an opportunity (to him)")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-xp-TEMPL (xp (% W::NP (W::lf (% ?p (w::class (? x ont::situation-root)))))))
     )
    ((LF-PARENT ONT::enable)
     (example "provide him with an opportunity")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-recipient-affected-templ (xp (% w::pp (w::ptype w::with))))
     (meta-data :origin calo-ontology :entry-date 20050922 :change-date 20090501 :comments vn2-integration)
     )

    #||  ((LF-PARENT ONT::supply)
     (example "the battery provides a spark (to the engine)")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-goal-optional-TEMPL)
     (meta-data :origin step :entry-date 20080626 :change-date 20090501 :comments nil)
     )
    ((LF-PARENT ONT::supply)
     (example "the battery provides the engine with power")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-recipient-affected-templ (xp (% w::pp (w::ptype w::with))))
     (meta-data :origin step :entry-date 20080626 :change-date 20090501 :comments nil)
     )||#
    )
   )
))

