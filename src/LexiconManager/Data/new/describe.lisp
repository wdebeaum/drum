;;;;
;;;; W::describe
;;;;

(define-words :pos W::v 
 :words (
  (W::describe
   (wordfeats (W::morph (:forms (-vb) :nom w::description)))
   (SENSES
    ((LF-PARENT ONT::describe)
     (example "he described the book")
     (meta-data :origin calo :entry-date 20041103 :change-date 20090506 :comments caloy2)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::describe)
     (example "I described it to be broken")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
    )
        )
   )
))

