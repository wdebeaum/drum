;;;;
;;;; W::itch
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::itch
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (example "my leg itches")
     (LF-PARENT ONT::experiencer-obj)
     (TEMPL affected-templ)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (example "this sweater itches [me]")
     (LF-PARENT ONT::evoke-physical-irritation)
     (TEMPL agent-affected-xp-optional-templ)
     )
    )
   )
))

