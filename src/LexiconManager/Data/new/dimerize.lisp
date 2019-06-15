;;;;
;;;; W::dimerize
;;;;

(define-words :pos W::v 
 :words (
  (W::dimerize
   (SENSES
    ((LF-PARENT ONT::BIND-INTERACT)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (example "The protein dimerizes with itself" "NF45 dimerizes with NF90")
     )

    ((LF-PARENT ONT::BIND-INTERACT)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
; the AFFECTED really should be plural, but we need to allow e.g., "Ras dimerizes"
     (TEMPL AGENT-RESULT-XP-PP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? w::to)))))
     (example "The proteins dimerize (to a dimer).")
     )

    )
   )
))

