;;;;
;;;; W::believe
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::believe
   (wordfeats (W::morph (:forms (-vb) :nom W::belief)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("believe%2:31:04:"))
     (example "I believe that going along the coast is faster")
     (LF-PARENT ONT::BELIEVE)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::believe)     
     (example "they believe her to have cancer")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-objcontrol-templ)
     )
     ((LF-PARENT ONT::BELIEVE)     
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-neutral-xp-templ)
     (example "I believe the hypothesis")
      )
    ((LF-PARENT ONT::TRUST)    
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-templ)
     (example "I believe")
     (preference .96)
     )
     ((LF-PARENT ONT::TRUST)     
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-neutral-xp-templ)
     (example "I believe him")
      )
    )
   )
))

