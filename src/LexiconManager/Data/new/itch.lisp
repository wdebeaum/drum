;;;;
;;;; W::itch
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::itch
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (example "my leg itches")
     ;(LF-PARENT ONT::experiencer-obj)
     (LF-PARENT ONT::PHYSICAL-SENSATION)
     (TEMPL experiencer-templ)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (example "this sweater itches [me]")
     (LF-PARENT ONT::evoke-physical-irritation)
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL)
     )
    )
   )
))

