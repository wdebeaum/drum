;;;;
;;;; W::journey
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::journey
   (SENSES
    ((meta-data :origin trips :entry-date 20090401 :change-date nil :wn  ("trip%1:04:00") :comments s11)
     (LF-PARENT ONT::TRIP)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::journey
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("journey%2:38:00" "journey%2:38:01"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

