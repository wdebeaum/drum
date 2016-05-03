;;;;
;;;; W::revive
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::revive
   (SENSES
    #||((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the doctor revived him")
      )||#
     ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the elixir revived him")
     (templ agent-affected-xp-templ)
      )
    ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ affected-templ)
     (example "he revived")
     )
    )
   )
))

