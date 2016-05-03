;;;;
;;;; W::SEPARATE
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::SEPARATE
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ont::separation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-AFFECTED-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (EXAMPLE "separate the crews")
     )
    ((LF-PARENT ONT::separation)
     (TEMPL agent-affected-as-comp-templ (xp (% w::pp (w::ptype (? t w::from)))))
     )

    ((LF-PARENT ONT::separation)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-plural-TEMPL)
     (example "They separate")
     )

    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::SEPARATE
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("separate%3:00:00"))
     (EXAMPLE "They are separate [from each other]")
     (LF-PARENT ONT::DIFFERENT)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::FROM))))
     )
    )
   )
))

