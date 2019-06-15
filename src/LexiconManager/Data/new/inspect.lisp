;;;;
;;;; W::inspect
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::inspect
     (wordfeats (W::morph (:forms (-vb) :nom w::inspection)))
   (SENSES
    ((EXAMPLE "inspect an object")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin lou :entry-date 20040311 :change-date nil :comments lou-sent-entry :vn ("investigate-35.4") :wn ("inspect%2:38:00" "inspect%2:39:00"))
     )
    )
   )
))

