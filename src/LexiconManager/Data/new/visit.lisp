;;;;
;;;; W::visit
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::visit
    (wordfeats (W::morph (:forms (-vb) :past W::visited :ing W::visiting)))
   (SENSES
    
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments chf-dialogues)
     (LF-PARENT ONT::Social-activity)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he was visiting yesterday")
     (TEMPL AGENT-TEMPL)
     )
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments chf-dialogues)
     (LF-PARENT ONT::Social-activity)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he was visiting his parents yesterday")
     )
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments chf-dialogues)
     (LF-PARENT ONT::Social-activity)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he was visiting with his parents yesterday")
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    )
   )
))

