;;;;
;;;; W::adjacent
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::adjacent
   (SENSES
    ((LF-PARENT ONT::CONNECTED-VAL)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("adjacent%5:00:00:connected:00" "adjacent%5:00:00:close:01") :comments nil)
     (EXAMPLE "combine adjacent cells")
     (TEMPL adj-theme-templ)  
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((W::ADJACENT w::to)
   (SENSES
    ((LF-PARENT ONT::adjacent)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

