;;;;
;;;; W::deal
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::deal
   (wordfeats (W::morph (:forms (-vb) :past W::dealt :nom w::deal)))
   (SENSES
    (;;(LF-PARENT ONT::managing)
     (lf-parent ont::cope-deal) ;; 20120524 GUM change new parent
     (example "deal with it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% W::pp (W::ptype W::with))))
     )
    )
   )
))

