;;;;
;;;; W::pile
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::pile
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::amass)
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )
    )
   )
  ))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::pile w::up)
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::amass)
     (TEMPL affected-templ)
     )
    )
   )
  ))
