;;;;
;;;; W::attempt
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::attempt
   (wordfeats (W::morph (:forms (-vb) :nom W::attempt)))
   (SENSES
    ((LF-PARENT ONT::TRY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype (? ct W::s-to)))))
     (example "he attempted to climb mt everest")
     )
      ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("try-61"))
       (LF-PARENT ONT::TRY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (example "he attempted the puzzle")
     )
    )
   )
))

