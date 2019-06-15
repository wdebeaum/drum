;;;;
;;;; w::paste
;;;;

(define-words :pos W::n
 :words (
  (w::paste
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL count-PRED-TEMPL)
    (example "antibiotic ointment")
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::paste
   (SENSES
    ((LF-PARENT ONT::put)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin task-learning :entry-date 20050823 :change-date 20050824 :comments nil)
     (example "paste the graph into the document")
     )
    )
   )
))

