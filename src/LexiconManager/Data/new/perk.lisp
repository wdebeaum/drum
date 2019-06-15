;;;;
;;;; W::perk
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::perk (w::up))
   (SENSES
    #||((LF-PARENT ONT::reviving) ;; subsumed by cause role
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "his friends perked him up")
      )||#
     ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the elixir perked him up")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
    ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ affected-templ)
     (example "he perked up")
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::perk
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::steep)
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

