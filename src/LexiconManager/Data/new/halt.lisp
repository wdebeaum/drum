;;;;
;;;; W::halt
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::halt
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("stop-55.4") :wn ("halt%2:38:01" "halt%2:38:05" "halt%2:41:00"))
     (LF-PARENT ONT::stop)
     (example "he halted the truck")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like terminate
      )
    ((EXAMPLE "he halted the proceedings")
     (LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     )
    ((EXAMPLE "the truck halted")
     (LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     )
    )
   )
))

