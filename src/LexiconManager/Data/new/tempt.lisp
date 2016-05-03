;;;;
;;;; W::tempt
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::tempt
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He tempted him [to run for office]")     
     )
    ((LF-PARENT ont::provoke)
     (TEMPL agent-affected-xp-templ)  ; like annoy,bother,concern,hurt
     (example "he tempted fate")
     )
    ;; he tempted him into running for office
    )
   )
))

