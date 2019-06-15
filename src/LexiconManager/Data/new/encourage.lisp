;;;;
;;;; W::encourage
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::encourage
   (SENSES
    ((EXAMPLE "He encourages good behavior")
     (LF-PARENT ONT::encourage)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("advise-37.9") :wn ("encourage%2:37:00"))
     )
    ((EXAMPLE "His financial success encouraged him to look for a wife")
     (LF-PARENT ONT::encourage)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("advise-37.9") :wn ("encourage%2:37:00"))
     )
    )
   )
))

