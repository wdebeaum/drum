;;;;
;;;; W::suggest
;;;;

(define-words :pos W::v 
 :words (
  (W::suggest
   (SENSES    
    ;;;; I suggested that ...
    ((LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL (xp (% W::cp (W::ctype (? ctp W::s-finite w::s-that-subjunctive)))))
     )
    ((meta-data :origin calo :entry-date 20040121 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     (example "He suggested a plan")
     )
    #||(;;(LF-PARENT ONT::SUGGEST)
     (lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL THEME-ASSOCIATED-INFORMATION-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (example "the evidence suggests that ...")
     )||#
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result suggested that the gene activates the protein")
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    (
     (LF-PARENT ONT::CORRELATION)
     (example "The results suggestd the theory")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )


    )
   )
))

