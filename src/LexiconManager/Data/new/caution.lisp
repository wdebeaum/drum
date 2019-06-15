;;;;
;;;; W::CAUTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CAUTION
   (SENSES
    ((LF-PARENT ONT::cautiousness-scale) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::caution
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::WARN)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL) ; like warn
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::WARN)
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     )
    )
   )
))

