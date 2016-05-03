;;;;
;;;; W::conjecture
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::conjecture
   (wordfeats (W::morph (:forms (-vb) :nom W::conjecture)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (TEMPL neutral-neutral-xp-templ)
     )

    )
   )
))

