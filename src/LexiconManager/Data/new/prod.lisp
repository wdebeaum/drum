;;;;
;;;; W::prod
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::prod
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He pressured him [to run for office]")  
     )
    ((LF-PARENT ONT::PUSH)
     (SEM (F::cause F::agentive) (F::aspect F::unbounded) (F::time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

