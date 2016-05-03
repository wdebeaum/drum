;;;;
;;;; W::prove
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::prove
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "this diagram proves that it works")
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like confirm
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "he proved the theorem")
     (TEMPL agent-theme-xp-templ) 
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::show)
     (example "I proved it to be broken")
     (TEMPL agent-effect-affected-objcontrol-templ)
    )

    ((LF-PARENT ONT::show)
     (example "we prove in this paper that it works")
     (preference .98)
     (TEMPL agent-located-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
 )
    (
     (LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )
    
    )
   )
))

