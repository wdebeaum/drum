;;;;
;;;; W::prompt
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::prompt
   (wordfeats (W::morph (:forms (-vb) :nom W::prompt)))
   (SENSES
    ((EXAMPLE "prompt (the user) for a password")
     (LF-PARENT ONT::REMIND)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-OPTIONAL-ADDRESSEE-THEME-TEMPL) ;should addressee be optional?
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     )
    )
   )
))

