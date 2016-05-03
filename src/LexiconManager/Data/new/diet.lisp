;;;;
;;;; W::diet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::diet
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("diet%1:13:00"))
     (LF-PARENT ONT::eating-plan)
     )
    ((meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil :wn ("diet%1:13:00"))
     (LF-PARENT ONT::food)
     (example "are you eating a balanced diet")
     (preference .96)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::diet
   (SENSES
    ((LF-PARENT ONT::dieting)
     (example "he is dieting")
     (meta-data :origin cardiac :entry-date 20081223 :change-date nil :comments LM-vocab)
     (templ agent-templ)
     )
    )
   )
))

