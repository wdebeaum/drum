;;;;
;;;; W::sign
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sign
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("sign%1:10:03"))
     (LF-PARENT ONT::information)
     (example "a sign of cardiac disease")
     )
    ((meta-data :wn ("sign%1:26:00"))
     (LF-PARENT ONT::medical-symptom)
     (example "a sign of cardiac disease")
     )
    ((meta-data :origin navigation :entry-date 20080902 :change-date nil :comments nil :wn ("sign%1:06:01"))
     (LF-PARENT ONT::manufactured-object)
     (example "a stop sign")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (w::sign
   (SENSES
     ((LF-PARENT ONT::write)
      (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil :vn ("image_impression-25.1") :wn ("sign%2:32:02" "sign%2:41:00"))
      (example "sign the requisition form")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-THEME-XP-TEMPL)
     )
    )
   )
))

