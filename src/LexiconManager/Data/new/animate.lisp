;;;;
;;;; w::animate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (w::animate
    (SENSES
    #||((LF-PARENT ONT::reviving)   ;; subsumed by cause role
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the doctor brought him to")
      )||#
     ((LF-PARENT ONT::evoke-liveliness)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "The bright light animated me.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     )
   )
))

