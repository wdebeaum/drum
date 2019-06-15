;;;;
;;;; w::dress
;;;;

(define-words :pos W::n
 :words (
  (w::dress
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::dress
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (EXAMPLE "he dressed quickly")
     (LF-PARENT ONT::PUT-ON)
     (templ agent-templ)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (EXAMPLE "he dressed the child")
     (LF-PARENT ONT::PUT-ON)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

