;;;;
;;;; W::passive
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::passive
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::INACTIVE)
     (SEM (F::GRADABILITY F::+))
     (example "passive listener")
     (meta-data :origin calo-ontology :entry-date 20060713 :change-date 20090731 :wn ("passive%3:00:01") :comments ma-request)
     )
    )
   )
))

