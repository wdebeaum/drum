;;;;
;;;; W::digest
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::digest
   (SENSES
    ((EXAMPLE "he digested the meal")
     (LF-PARENT ONT::bodily-process)
     (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
     (TEMPL agent-affected-xp-TEMPL)
     )
    ((EXAMPLE "he digested in silence")
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-TEMPL)
     )
    ((LF-PARENT ONT::CONSUME)
     (TEMPL agent-affected-xp-TEMPL)
     )
    )
   )
))

