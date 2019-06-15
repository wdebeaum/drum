;;;;
;;;; W::prove
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::prove
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "this diagram proves that it works")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like confirm
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "he proved the theorem")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::show)
     (example "I proved it to be broken")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
    )

    ((LF-PARENT ONT::show)
     (example "we prove in this paper that it works")
     (preference .98)
     (TEMPL AGENT-FORMAL-LOCATION-2-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite))))
 )
    (
     (LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    
    )
   )
))

