;;;;
;;;; W::advise
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::advise
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::advise)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL) ; like warn
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::advise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "I'll advise him to do it")
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     )
    )
   )
))

