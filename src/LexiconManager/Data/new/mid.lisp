;;;;
;;;; W::MID
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::MID w::POINT)
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::CENTER);waypoint)
     (example "the midpoint of the road")
;     (TEMPL PART-OF-RELN-TEMPL)
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

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::mid
   (SENSES
    ((meta-data :origin step :entry-date 20080528 :change-date nil :comments nil)
     (lf-parent ont::middle-val)
     (example "the mid 80's" "a mid life crises")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::mid-
   (SENSES
    ((meta-data :origin step :entry-date 20080528 :change-date nil :comments nil)
     (lf-parent ont::middle-val)
     (templ prefix-adj-templ)
     (example "mid-January")
     )
    )
   )
))


