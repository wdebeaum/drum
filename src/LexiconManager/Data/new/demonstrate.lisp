;;;;
;;;; W::demonstrate
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::demonstrate
   (wordfeats (W::morph (:forms (-vb) :nom w::demonstration )))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like confirm
     )

    ((LF-PARENT ONT::show)
     (example "we demonstrate in this paper that it works")
     (preference .98)
     (TEMPL agent-located-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     )
    ((LF-PARENT ONT::show)
     (example "I demonstrated it to be broken")
     (TEMPL agent-effect-affected-objcontrol-templ))
    
    ((EXAMPLE "demonstrate the procedure (to him)")
     (LF-PARENT ONT::show)
     ;; this verb doesn't participate in the alternation "demonstrate him the procedure"
     (TEMPL agent-theme-to-addressee-optional-templ) 
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("transfer_mesg-37.1"))
     )

    ((LF-PARENT ONT::correlation)
     (example "The results demonstrated that the gene activates the protein")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
    )
    ((LF-PARENT ONT::correlation)
     (example "The results demonstrated the theory")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
    )

   ))
))

