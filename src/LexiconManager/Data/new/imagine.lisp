;;;;
;;;; W::imagine
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::imagine
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("appoint-29.1-2"))
     (LF-PARENT ONT::cogitation)
     (example "I must have imagined it")
     (TEMPL agent-theme-xp-templ) ; like consider,rate
     (PREFERENCE 0.96)
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s7)
     (LF-PARENT ONT::BELIEVE)
     (example "I imagine that this will work")
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    )
   )
))

