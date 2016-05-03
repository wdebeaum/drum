;;;;
;;;; W::brief
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::brief
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::describe)
     (TEMPL agent-addressee-theme-optional-templ) ; like inform
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::describe)
     (TEMPL agent-addressee-associated-information-templ) ; like notify
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::describe)
     (TEMPL agent-addressee-theme-optional-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like notify
     )
    )
   )
))

