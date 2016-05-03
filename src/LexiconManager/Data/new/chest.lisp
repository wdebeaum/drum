;;;;
;;;; w::chest
;;;;

(define-words :pos W::n
 :words (
  ((w::chest w::discomfort)
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::CHEST
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

