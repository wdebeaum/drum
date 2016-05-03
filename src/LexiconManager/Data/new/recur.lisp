;;;;
;;;; W::recur
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::recur
   (wordfeats (W::morph (:forms (-vb) :nom W::recurrence)))
   (SENSES
    ((LF-PARENT ONT::REPEAT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ theme-templ)
     (example "the pattern recurs in this document")
     (meta-data :origin calo-ontology :entry-date 20060713 :change-date nil :comments caloy3)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::recur
   (wordfeats (W::morph (:forms (-vb) :nom w::recurrence)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3") :wn ("recur%2:30:00"))
     (LF-PARENT ONT::occurring)
     (TEMPL neutral-templ) ; like occur,happen
     )
    )
   )
))

