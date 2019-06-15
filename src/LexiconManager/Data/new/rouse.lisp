;;;;
;;;; W::rouse
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::rouse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)  ; like dare
     (example "He roused him [to run for office]")  
     )
    ((LF-PARENT ont::provoke)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::in w::among))))) ; like annoy,bother,concern,hurt
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "the lecture roused anger [in the crowd]")
     )
    )
   )
))

