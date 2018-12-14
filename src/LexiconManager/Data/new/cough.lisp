;;;;
;;;; W::COUGH
;;;;

(define-words :pos W::n
 :words (
  (W::COUGH
  (senses
   ((meta-data :wn ("cough%1:26:00"))
    (LF-PARENT ONT::cough)
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
     (LF-PARENT ont::exhale-forcefully)
     (TEMPL affected-templ) ; like laugh
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::cough (w::up))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20090403 :change-date nil :comments nil)
     (LF-PARENT ont::exhale-forcefully)
     (TEMPL affected-templ)
     )
    )
   )
))

(define-words :pos W::v
 :words (
   ((W::cough (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ont::exhale-forcefully)
     (TEMPL affected-affected-templ) ; like vomit
     )
    )
   )
))

