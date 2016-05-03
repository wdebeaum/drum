;;;;
;;;; W::notate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::notate
    (wordfeats (W::morph (:forms (-vb) :nom w::notation)))
   (SENSES   
     ((meta-data :origin calo-ontology :entry-date 20060424 :change-date nil :comments iris)
      (LF-PARENT ONT::write)
      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (EXAMPLE "annotate the text")
     )
    )
   )
))

