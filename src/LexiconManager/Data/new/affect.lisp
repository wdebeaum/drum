;;;;
;;;; W::affect
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::affect
   (SENSES
    ((meta-data :origin beetle :entry-date 20081107 :change-date nil :comments nil)
     (LF-PARENT ONT::objective-influence)
     (EXAMPLE "the plan affects another plan" "the plan affects the city")
     (TEMPL agent-affected-xp-templ) 
     )
    ))
))

