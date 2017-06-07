;;;;
;;;; W::SWITCH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SWITCH
   (SENSES
    ((meta-data :origin caet :entry-date 20120102 :change-date nil :comments nil)
     (example "press the switch to turn it on")
 ;    (LF-PARENT ONT::device-component)
     (lf-parent ont::switch)
     )
    )
   )
))

#|
(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::switch (W::off))
   (SENSES
    ((LF-PARENT ont::turn-off)
     (example "switch off the radio")
     (meta-data :origin beetle :entry-date 20081106 :change-date nil :comments nil)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ;; turning off things like water means really turning off whatever source produces them
    ((LF-PARENT ont::turn-off)
     (meta-data :origin beetle :entry-date 20081106 :change-date nil :comments nil)
     (example "switch off the water") 
     (templ agent-affected-xp-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (preference 0.95) ;; to interpret turn it off primarily as agent-theme
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::switch (W::on))
   (SENSES
    ((LF-PARENT ont::start-object)
     (example "switch on the radio")
     (meta-data :origin beetle :entry-date 20081106 :change-date nil :comments nil)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
   ))))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::switch (w::over))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("convert-26.6.2-1"))
     (LF-PARENT ONT::replacement)
 ; like switch
     )
    )
   )
))
|#

