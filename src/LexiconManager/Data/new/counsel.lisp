;;;;
;;;; W::counsel
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::counsel
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL agent-addressee-theme-optional-templ) ; like warn
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL agent-addressee-theme-objcontrol-req-templ (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     )
    )
   )
))

