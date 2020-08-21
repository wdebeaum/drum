;;;;
;;;; W::photographic
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::photographic
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("photographic%3:01:00") :comments caloy3)
     (LF-PARENT ONT::artifact-property-val) ;medium)
     (example "photographic film") ;; like electromagnetic, holographic
     )

     ((LF-PARENT ONT::representation-method-val)
     (example "they showed it in a photographic manner")
     )

    )
   )
))

