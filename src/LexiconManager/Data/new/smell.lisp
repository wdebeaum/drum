;;;;
;;;; w::smell
;;;;

(define-words :pos W::n
 :words (
  (w::smell
  (senses
   ((LF-PARENT ONT::smell-scale)
    (example "this is far worse than the smell(iness) of dirty socks")
    (TEMPL MASS-PRED-TEMPL)
    )
   ((LF-PARENT ONT::ability-to-smell)
    (example "my sense of smell is shot from congestion")
    (TEMPL MASS-PRED-TEMPL)
    )
   ((LF-PARENT ONT::perceivable-smell-property)
    (example "this fish has a strong smell (odor)")
    (TEMPL MASS-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::smell
   (SENSES
     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("stimulus_subject-30.4") :wn ("smell%2:39:00" "smell%2:39:02"))
     (LF-PARENT ONT::appears-to-have-property)
     (example "it smells like trouble")
     (TEMPL NEUTRAL-FORMAL-SUBJCONTROL-TEMPL) ; like look
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
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-A-TEMPL (xp (% W::VP (W::vform (? vf2 W::base w::ing)))))
     )
    )
   )
))

