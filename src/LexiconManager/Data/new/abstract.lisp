;;;;
;;;; W::abstract
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::abstract
   (SENSES
    ((LF-PARENT ONT::composition)
     (EXAMPLE "read the abstract")
     (meta-data :origin calo-ontology :entry-date 20060608 :change-date nil :wn ("abstract%1:10:00") :comments plow-req)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::abstract
   (SENSES
    ((LF-PARENT ONT::SUMMARIZE)
     (example "he abstracted the paper")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% w::NP (w::refl -))))
     )
   ((LF-PARENT ONT::CAUSE-COME-FROM)
     (example "he abstracted himself from the situation")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ (xp (% w::NP (w::refl +))))
     )
   ))
  ))

