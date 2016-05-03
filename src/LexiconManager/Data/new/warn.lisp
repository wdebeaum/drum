;;;;
;;;; W::warn
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::warn
   (wordfeats (W::morph (:forms (-vb) :nom W::warning)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090506 :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::warn)
     (TEMPL agent-addressee-theme-objcontrol-req-templ (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::warn)
     (TEMPL agent-addressee-theme-OPTIONAL-templ)
     )
    )
   )
))

