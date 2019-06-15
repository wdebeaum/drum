;;;;
;;;; W::deliver
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::deliver
   (wordfeats (W::morph (:forms (-vb) :past W::delivered :ing W::delivering :nom w::delivery)))
   (SENSES
    ((LF-PARENT ONT::DELIVER)
     (example "the truck delivered the oranges to avon")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    #||((LF-PARENT ONT::deliver)
     (example "deliver the oranges to boston")
     )||#
    ((LF-PARENT ONT::CAUSE-EFFECT)
     (example "deliver a blow")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

