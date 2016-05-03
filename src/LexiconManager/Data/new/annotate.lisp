;;;;
;;;; W::annotate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::annotate
    (wordfeats (W::morph (:forms (-vb) :nom w::annotation)))
   (SENSES   
     ((meta-data :origin calo-ontology :entry-date 20060424 :change-date nil :comments iris :vn ("image_impression-25.1"))
      (LF-PARENT ONT::write)
      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (EXAMPLE "annotate the text")
     )
    )
   )
))

