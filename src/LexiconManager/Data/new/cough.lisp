;;;;
;;;; W::COUGH
;;;;

(define-words :pos W::n
 :words (
  (W::COUGH
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::cough
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2") :wn ("cough%2:29:00"))
     (LF-PARENT ont::bodily-process)
     (TEMPL agent-templ) ; like laugh
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::cough (w::up))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20090403 :change-date nil :comments nil)
     (LF-PARENT ont::bodily-process)
     (TEMPL agent-templ)
     )
    )
   )
))

(define-words :pos W::v
 :words (
   ((W::cough (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ont::bodily-process)
     (TEMPL agent-affected-xp-templ) ; like vomit
     )
    )
   )
))

