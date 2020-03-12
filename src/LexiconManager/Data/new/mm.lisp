;;;;
;;;; W::mm
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::mm ;; alternate plural
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    (;(LF-PARENT ONT::length-unit)
     (LF-PARENT ONT::MM)
     (TEMPL ATTRIBUTE-UNIT-PLURAL-TEMPL)
     ;(LF-FORM W::millimeter)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("mm%1:23:00") :comments nil)
     )
    ) 
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::mm W::hm)
   (SENSES
    ((LF (W::mmhm))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_ACK))
     )
    )
   )
))

