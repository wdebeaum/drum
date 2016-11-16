;;;;
;;;; W::FUEL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FUEL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("fuel%1:27:00"))
     (LF-PARENT ONT::LIQUID-SUBSTANCE)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::fuel
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     (LF-PARENT ONT::fill-container)
     (example "fuel the car with unleaded gasoline")
     (TEMPL AGENT-GOAL-affected-TEMPL)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090521 :change-date nil :comments nil)
     (LF-PARENT ONT::fill-container)
     (example "The tanker fueled in Iran")
     (templ goal-templ)
     (preference .97) 
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090521 :change-date nil :comments nil)
     (LF-PARENT ONT::cause-stimulate)
     (example "oxygen fueled the fire" "he fueled the debate with his interest in the presidency")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

