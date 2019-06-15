;;;;
;;;; W::write
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::write
   (wordfeats (W::morph (:forms (-vb) :past W::wrote :pastpart W::written :ing W::writing)))
   (SENSES
    ((EXAMPLE "Write the letter")
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (prototypical-word t)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    (
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (EXAMPLE "he wrote")
     (PREFERENCE 0.98) ;; prefer transitive sense
     )

    ((LF-PARENT ONT::NONVERBAL-SAY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (EXAMPLE "he wrote that he couldn't come")
     )
    ((LF-PARENT ONT::NONVERBAL-SAY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     (EXAMPLE "he wrote green")
     (PREFERENCE 0.92) ;; only as a last resort
     )
    )
   )
))

