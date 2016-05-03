;;;;
;;;; W::PAD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PAD
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("pad%1:06:02"))
     (LF-PARENT ONT::transportation-facility)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::pad
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("pad%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

