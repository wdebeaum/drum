;;;;
;;;; W::coincide
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::coincide
   (SENSES
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-plural-templ)
     (meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("amalgamate-22.2-2-1" "correlate-86") :wn ("coincide%2:30:00"))
     )
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ (xp (% W::pp (W::ptype W::with))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("amalgamate-22.2-2-1" "correlate-86") :wn ("coincide%2:30:00"))
     )
    )
   )
))

