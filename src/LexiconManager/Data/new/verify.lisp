;;;;
;;;; W::verify
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::verify
    (wordfeats (W::morph (:forms (-vb) :nom w::verification)))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050817 :change-date nil :comments nil)
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    ((meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :comments caloy3)
     (LF-PARENT ONT::scrutiny)
     (example "verify that he does it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     )
    )
   )
))

