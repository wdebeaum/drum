;;;;
;;;; W::DAMAGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DAMAGE
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :wn ("damage%1:11:00"))
     (LF-PARENT ONT::trouble)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::damage
   (SENSES
    #||((LF-PARENT ONT::BREAK-OBJECT)  ;; subsumed by CAUSE template
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he damaged the computer")
     )||#
    (;(LF-PARENT ont::break-object)
     (LF-PARENT ont::damage)
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     ;;(SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     (example "it damaged the computer")
     )
    )
   )
))

