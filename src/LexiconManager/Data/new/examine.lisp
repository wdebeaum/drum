;;;;
;;;; W::examine
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::examine
   (wordfeats (W::morph (:forms (-vb) :nom w::examination)))
   (SENSES
    ((EXAMPLE "I want amigo to start examining this area")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin lou :entry-date 20031105 :vn ("investigate-35.4") :wn ("examine%2:31:00" "examine%2:32:01" "examine%2:39:00"))
     )
    )
   )
))

