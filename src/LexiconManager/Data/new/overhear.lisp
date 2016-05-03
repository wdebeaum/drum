;;;;
;;;; W::overhear
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::overhear
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("sight-30.2") :wn ("overhear%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL experiencer-neutral-templ) ; like observe,view,watch
     )
    ((meta-data :origin trips :entry-date 20090331 :change-date nil :comments missing-sense)
     (LF-PARENT ONT::active-perception)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "he overheard him speak about it")
     (TEMPL experiencer-action-objcontrol-templ (xp (% W::VP (W::vform W::base))))
     )
    )
   )
))

