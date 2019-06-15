;;;;
;;;; W::adjust
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::adjust
     (wordfeats (W::morph (:forms (-vb) :nom w::adjustment)))
   (SENSES
    ((LF-PARENT ONT::adjust)
     (example "adjust my dosage")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    #||((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (LF-PARENT ONT::adjust)
     (example "it adjusted with time")
     (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
     )||#
    )
   )
))

