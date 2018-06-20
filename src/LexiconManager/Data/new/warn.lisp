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
    ((lf-parent ont::warn)
     (example "he warned that they woll starve")
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     (meta-data :origin step :entry-date 20080627 :change-date nil :comments nil :vn ("say-37.7") :wn ("claim%2:32:00"))
     )
    (
     (LF-PARENT ONT::encodes-message)
     (example "The book warned that they will starve.")
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    
    
    )
   )
))

