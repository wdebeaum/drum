;;;;
;;;; W::thumb
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::thumb W::drive)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments caloy3)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::THUMB
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

