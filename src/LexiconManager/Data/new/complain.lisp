;;;;
;;;; W::complain
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::complain
   (wordfeats (W::morph (:forms (-vb) :nom w::complaint)))
   (SENSES
    ((meta-data :origin plot :entry-date 20080413 :change-date 20090508 :comments nil :vn ("chit_chat-37.6") :wn ("argue%2:32:00"))
     (LF-PARENT ONT::complain)
     (example "he complained")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::complain)
     (example "he complained to her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL
            (xp1 (% w::pp (w::ptype w::to))))
     )
    ((LF-PARENT ONT::complain)
     (example "he complained about it [to her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL
	     (xp2 (% w::pp (w::ptype w::to))))
     )
    )
   )
))

