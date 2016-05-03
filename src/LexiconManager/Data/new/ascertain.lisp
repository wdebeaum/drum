;;;;
;;;; W::ascertain
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::ascertain
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("discover-84"))
     (LF-PARENT ONT::determine)
 ; like discover
     (templ agent-theme-xp-templ)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("discover-84"))
     (LF-PARENT ONT::determine)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like discover
     )
    )
   )
))

