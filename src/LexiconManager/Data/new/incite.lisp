;;;;
;;;; W::incite
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::incite
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He incited him [to run for office]")  
     )
    ((LF-PARENT ont::provoke)
     (TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from w::in w::among)))))
     (example "His manner incited anger [in them]")  
     )
    )
   )
))

