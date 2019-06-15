;;;;
;;;; W::plot
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::plot
   (SENSES
    ((meta-data :origin step :entry-date 20080721 :change-date nil :comments nil)
     (LF-PARENT ONT::area-def-by-use)
     (example "a plot of land")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::plot
   (wordfeats (W::morph (:forms (-vb) :past W::plotted)))
   (SENSES
    ((EXAMPLE "plot those points")
     (LF-PARENT ONT::categorization)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-TEMPL)
     )
    )
   )
))

