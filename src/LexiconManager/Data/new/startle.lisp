;;;;
;;;; W::startle
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::startle
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("startle%2:37:00"))
     (LF-PARENT ont::evoke-surprise)
     (example "the thunder startled him [into action]")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
;     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-B-TEMPL (xp (% w::pp (w::ptype (? pt w::into))))) ; like annoy,bother,concern,hurt
     )
    )
   )
))

