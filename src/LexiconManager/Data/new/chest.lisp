;;;;
;;;; w::chest
;;;;

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

#|
(define-words :pos W::n
 :words (
  ((w::chest w::discomfort)
  (senses
   ((meta-data :wn ("chest_pain%1:26:00"))
    (LF-PARENT ONT::medical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))
|#