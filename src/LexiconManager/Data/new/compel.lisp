;;;;
;;;; W::compel
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::compel
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He compelled him [to run for office]")  
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from w::in)))))
     (templ agent-affected-xp-templ)
     (example "His manner compelled respect [from the audience]")  
     )
    )
   )
))

