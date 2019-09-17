;;;;
;;;; W::E-MAIL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::E w::punc-minus w::MAIL)
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024 :wn ("e-mail%1:10:00"))
     (LF-PARENT ONT::email)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ((W::E w::punc-minus w::MAIL)
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "e-mail the image to him" "e-mail him the message")
     (TEMPL AGENT-AFFECTED-TEMPL)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )

    ((LF-PARENT ONT::NONVERBAL-SAY)
     (example "I e-mailed all of partners to send what they did for the project")
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL)
     )

    )
   )
))

