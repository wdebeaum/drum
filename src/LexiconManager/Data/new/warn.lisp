;;;;
;;;; W::warn
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::warn
   (wordfeats (W::morph (:forms (-vb) ))) ;;:nom W::warning)))  This is redundant with the GERUND
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090506 :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::warn)
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::warn)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL)
     )
    ((lf-parent ont::warn)
     (example "he warned that they woll starve")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     (meta-data :origin step :entry-date 20080627 :change-date nil :comments nil :vn ("say-37.7") :wn ("claim%2:32:00"))
     )
    (
     (LF-PARENT ONT::encodes-message)
     (example "The book warned that they will starve.")
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    
    
    )
   )
))

