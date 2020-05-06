
;;;;
;;;; W::change
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::change
     (wordfeats (W::morph (:forms (-vb) :nom w::change :nomobjpreps (w::in w::of))))
   (SENSES
    ((LF-PARENT ONT::change)
     (example "change the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::change)
     (example "it changed")
     (templ affected-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::change)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (example "it changed in color / with time")
      (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
     )
    ))
  ))

