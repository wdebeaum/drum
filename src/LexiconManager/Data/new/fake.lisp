;;;;
;;;; W::fake
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::fake
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date 20090731 :wn ("fake%5:00:00:artificial:00") :comments nil)
     (LF-PARENT ONT::artificial)
     (TEMPL central-adj-TEMPL)
     (SEM (F::GRADABILITY +))
     (example "fake teeth")
     )
    ((LF-PARENT ONT::fake-val)
     (example "fake Rembrandt painting")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil :wn ("fake%5:00:00:counterfeit:00"))
     )
    )
   )
))

