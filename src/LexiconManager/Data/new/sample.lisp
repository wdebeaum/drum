;;;;
;;;; W::SAMPLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SAMPLE
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024)
     (example "take a soil sample")
     (LF-PARENT ONT::geo-sample)
     (TEMPL OTHER-RELN-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20041203 :change-date 20050817 :wn ("example%1:09:00") :comments lf-restructuring)
     (LF-PARENT ONT::example)
     (TEMPL OTHER-RELN-TEMPL)
     (example "try a free sample")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::SAMPLE
   (SENSES
    ((EXAMPLE "I am starting to sample fossils")
     (LF-PARENT ONT::sampling)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

