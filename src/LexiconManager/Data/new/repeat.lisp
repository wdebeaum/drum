;;;;
;;;; W::repeat
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::repeat
     (wordfeats (W::morph (:forms (-vb) :nom W::repetition)))
   (SENSES
    ;;;; repeat the statement/the information
    ((LF-PARENT ONT::REPEAT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
     ;;;; repeat that ...
    ((LF-PARENT ONT::REPEAT)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-that)))))
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )   
    )
   )
))

