;;;;
;;;; w::bathe
;;;;

(define-words :pos W::v :templ AGENT-TEMPL ;n :templ COUNT-PRED-TEMPL
 :words (
  (w::bathe ;bath
   (wordfeats (W::morph (:forms (-vb) :nom w::bath)))
   (SENSES
    ((meta-data :origin asma :entry-date 20111005)
     (LF-PARENT ONT::bath)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::bath w::tub)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :comments plow-req)
     (LF-PARENT ONT::fixture)
     )
    )
   )
))

