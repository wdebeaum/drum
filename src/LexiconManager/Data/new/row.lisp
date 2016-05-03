;;;;
;;;; W::row
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::row
   (SENSES
    ((LF-PARENT ONT::shape-object)
     (EXAMPLE "Move to the beginning of the row")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("row%1:17:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::row
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("nonvehicle-51.4.2") :wn ("row%2:38:00"))
     (LF-PARENT ONT::ride)
 ; like ride
     )
    )
   )
))

