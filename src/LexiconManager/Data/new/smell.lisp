;;;;
;;;; w::smell
;;;;

(define-words :pos W::n
 :words (
  (w::smell
  (senses
   ((LF-PARENT ONT::physical-sense)
    (TEMPL MASS-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::smell
   (SENSES
     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("stimulus_subject-30.4") :wn ("smell%2:39:00" "smell%2:39:02"))
     (LF-PARENT ONT::appears-to-have-property)
     (example "it smells like trouble")
     (TEMPL neutral-theme-complex-subjcontrol-templ) ; like look
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::ACTIVE-PERCEPTION)
     (SEM (F::Time-span F::extended))
     (TEMPL experiencer-NEUTRAL-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20090331 :change-date nil :comments missing-sense)
     (LF-PARENT ONT::active-perception)
     ;;(SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "he smelled the cake bake")
     (TEMPL experiencer-action-objcontrol-templ (xp (% W::VP (W::vform (? vf W::base w::ing)))))
     )
    )
   )
))

