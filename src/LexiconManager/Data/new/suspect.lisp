;;;;
;;;; W::suspect
;;;;

(define-words :pos W::V 
 :words (
  (W::suspect
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("believe%2:31:04:"))
     (example "I suspect that going along the coast is faster")
     (LF-PARENT ONT::expectation)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL experiencer-formal-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::expectation)     
     (example "they suspect her to have cancer")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-B-TEMPL)
     )

    ((LF-PARENT ONT::expectation)     
     (example "they suspect her of cheating")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL (xp2 (% w::pp (w::ptype w::of))))
     )
        
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::suppose)
     (TEMPL experiencer-templ) ; like guess
     )
    )
   )
))

