;;;;
;;;; W::shadow
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::shadow
   (SENSES
    ((LF-PARENT ONT::light)
     (EXAMPLE "set the shadow's color")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("shadow%1:15:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::shadow
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chase-51.6") :wn ("shadow%2:38:00"))
     (LF-PARENT ONT::pursue)
 ; like track
     )
    )
   )
))

