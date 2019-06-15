;;;;
;;;; W::assume
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::assume
   (wordfeats (W::morph (:forms (-vb) :nom W::assumption)))
   (SENSES
    ;;;; swier -- I assumed that there was enough space for the cargo.
    ((LF-PARENT ONT::ASSUME)
     (meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("assume%2:31:00"))
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::ASSUME)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-neutral-xp-templ)
     )
    )
   )
))

