;;;;
;;;; W::associate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::associate
    (wordfeats (W::morph (:forms (-vb) :nom w::association :nomsubjpreps (w::with) :nomobjpreps (w::between w::of))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (EXAMPLE "associate this voice note with site three")
     (LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::pp (W::ptype W::with))))
     )

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (EXAMPLE "This protein associates with another protein")
     )

    ((LF-PARENT ONT::RELATE)
     (TEMPL NEUTRAL-NEUTRAL1-NEUTRAL2-XP-TEMPL (xp (% W::pp (W::ptype W::with))))
     (EXAMPLE "The evidence associates X with Y.")
     )

    )
   )
))

