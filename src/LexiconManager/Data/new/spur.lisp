;;;;
;;;; W::spur
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::spur
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He spurred him [to run for office]")  
     )
    ((LF-PARENT ont::provoke)
     (TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::among))))) ; like annoy,bother,concern,hurt
     (example "the lecture spurred anger [in the crowd]")
     )
    )
   )
))

