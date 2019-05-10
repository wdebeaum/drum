;;;;
;;;; W::test
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::test
   (SENSES
    ((meta-data :origin calo :entry-date 20040119 :change-date nil :wn ("test%1:04:02") :comments caloy2)
     (LF-PARENT ONT::gathering-event)
     (example "I went took the test")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
 (W::test
     (wordfeats (W::morph (:forms (-vb) :nom w::test)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :vn ("investigate-35.4"))
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (example "test the plan")
     )
    ((meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :comments caloy3)
     (LF-PARENT ONT::scrutiny)
     (example "test whether it works")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
;     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     (TEMPL agent-formal-xp-templ)
     )
    )
   )
))

