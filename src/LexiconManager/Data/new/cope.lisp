;;;;
;;;; W::cope
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::cope
   (SENSES
    (;(LF-PARENT ONT::managing) ; like deal, (not entirely sure this is correct --wdebeaum)
     (lf-parent ont::cope-deal) ;; 20120524 GUM change new parent
     (example "cope with it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL neutral-neutral-TEMPL (xp (% W::pp (W::ptype W::with))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("cope-81-1"))
     )
    )
   )
))

