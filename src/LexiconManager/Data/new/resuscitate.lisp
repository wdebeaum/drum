;;;;
;;;; w::resuscitate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (w::resuscitate
    (SENSES
    #||((LF-PARENT ONT::reviving)   ;; subsumed by cause
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the doctor brought him to")
      )||#
     ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the elixir brought him to")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     )
   )
))

