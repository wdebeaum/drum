;;;;
;;;; W::command
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::command
   (wordfeats (W::morph (:forms (-vb) :nom w::command)))
   (SENSES
    ((LF-PARENT ONT::Command)
     (example "command him to do it")
     (meta-data :origin "verbnet-2.0" :entry-date 20060605 :change-date nil :comments nil :vn ("order-60-1"))
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::command)
     (example "he commands that you send this message")
     (meta-data :origin ptb :entry-date 20100421 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL (xp (% W::cp (W::ctype (? c w::s-that-subjunctive)))))
     )
    )
   )
))

