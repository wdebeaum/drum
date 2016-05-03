;;;;
;;;; W::infer
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::infer
   (wordfeats (W::morph (:forms (-vb) :3s W::guesses)))
   (SENSES
    ((LF-PARENT ONT::determine)
     (example "he inferred the answer")
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     (TEMPL agent-theme-xp-templ)
     )
     ((LF-PARENT ONT::determine)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "I inferred that it was time to take actionq")
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    )
   )
))

