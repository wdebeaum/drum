;;;;
;;;; W::incite
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::incite
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)  ; like dare
     (example "He incited him [to run for office]")  
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from w::in w::among)))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "His manner incited anger [in them]")  
     )
    )
   )
))

