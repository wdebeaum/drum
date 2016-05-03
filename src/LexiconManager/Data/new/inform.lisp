;;;;
;;;; W::inform
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::inform
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::tell)
     (TEMPL agent-addressee-associated-information-templ) ; like notify
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (LF-PARENT ONT::tell)
     (TEMPL agent-addressee-theme-optional-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like notify
     (PREFERENCE 0.96)
     )
    )
  )
))

