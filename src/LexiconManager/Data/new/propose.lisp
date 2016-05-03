;;;;
;;;; W::propose
;;;;

(define-words :pos W::v 
 :words (
  (W::propose
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s15)
     (LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-formal-XP-TEMPL (xp (% W::cp (W::ctype (? cpt W::s-finite w::s-to w::s-that-subjunctive)))))
     (example "he proposed that we make a plan")
     )
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s15)
     (LF-PARENT ONT::SUGGEST)
     ;;(lf-parent  ont::propose-recommend-suggest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-neutral-XP-TEMPL)
     (example "He proposed a plan")
     )
    
    )
   )
))

