;;;;
;;;; W::argue
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::argue
   (wordfeats (W::morph (:forms (-vb) :nom w::argument)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("argue%2:32:00"))
     (LF-PARENT  ONT::argue)
     (example "he argued")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT  ONT::argue)
     (example "he argued with her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL
            (xp1 (% w::pp (w::ptype w::with))))
     )
    ((LF-PARENT  ONT::argue)
     (example "he argued about it [with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     )
    )
   )
))

