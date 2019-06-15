;;;;
;;;; W::fail
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::fail
   (wordfeats (W::morph (:forms (-vb) :nom w::failure)))
   (SENSES
    ((lf-parent ont::fail)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he failed to see the problem")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to))))
     (meta-data :origin calo :entry-date 20041123 :change-date nil :comments caloy2)
     )
    ((LF-PARENT ONT::failing)
     (example "he failed the student")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin calo :entry-date 20041123 :change-date nil :comments caloy2)
     )
     ((LF-PARENT ONT::fail)
     (example "he failed the test")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL) 
     (meta-data :origin calo :entry-date 20041123 :change-date nil :comments caloy2)
     )
     ((LF-PARENT ONT::fail)
      (example "he failed")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-TEMPL) 
      )

     ((LF-PARENT ONT::fail)
      (example "he failed him in singing")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype (? pt w::at w::in)))))
      )

     ((LF-PARENT ONT::fail)
      (example "he failed at singing")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype (? pt w::at w::in)))))
      )
     
    ))
))

