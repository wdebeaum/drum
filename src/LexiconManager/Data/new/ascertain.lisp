;;;;
;;;; W::ascertain
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::ascertain
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("discover-84"))
     (LF-PARENT ONT::determine)
 ; like discover
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("discover-84"))
     (LF-PARENT ONT::determine)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-that)))) ; like discover
     )
    )
   )
))

