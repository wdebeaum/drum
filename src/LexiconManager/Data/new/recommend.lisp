;;;;
;;;; W::recommend
;;;;

(define-words :pos W::v 
 :words (
  (W::recommend
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("judgement-33") :wn ("recommend%2:32:00"))
     (LF-PARENT ont::suggest)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (TEMPL agent-neutral-xp-templ) ; like thank but changed to ont::suggest
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (example "he recommends that you get enough sleep")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-formal-XP-TEMPL (xp (% W::cp (W::ctype (? ctp W::s-finite w::s-that-subjunctive)))))
     )
    ;; adding the optional addressee -- otherwise this fails
    ((meta-data :origin calo :entry-date 20040504 :change-date 20090130 :comments html-purchasing-corpus)
     (LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (example "I recommend this computer (to you)")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-to-addressee-optional-TEMPL)
     )
    )
   )
))

