;;;;
;;;; W::MIDPOINT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MIDPOINT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::CENTER);waypoint)
     (example "the midpoint of the road")
 ;    (TEMPL PART-OF-RELN-TEMPL)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    #|
    ((LF-PARENT ONT::TIME-POINT)
     (example "the midpoint of the meeting/lesson")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
     (TEMPL GEN-PART-OF-RELN-ACTION-TEMPL)
     )
    ((LF-PARENT ONT::TIME-POINT)
     (example "the midpoint of the year")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
     (TEMPL GEN-PART-OF-RELN-INTERVAL-TEMPL)
     )
    |#
    )
   )
))

