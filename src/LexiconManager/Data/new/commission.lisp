;;;;
;;;; W::commission
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::commission
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     ;(LF-PARENT ont::provoke)
     (LF-PARENT ont::request)
     (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)  ; like dare
     (example "He commissioned him [to run for office]")  
     )
    (;(LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from w::by)))))
     (LF-PARENT ont::request)
     (templ agent-neutral-xp-templ)
     (example "He commissioned a portrait [from the artist]")  
     )
    )
   )
))

