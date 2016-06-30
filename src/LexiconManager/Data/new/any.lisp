;;;;
;;;; W::ANY
;;;;

(define-words :pos W::ADV
 :words (
  ;; negative polarity!
  ((W::ANY W::LONGER)
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-S-POST-TEMPL)
     (example "he doesn't do it any longer")
     )
    ;; less common, but grammatical
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-PRE-TEMPL)
     (example "he doesn't any longer do it")
     (preference .96) 
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::ANY
   (wordfeats (W::status W::indefinite) (w::npmod +) (w::negatable +) (W::mass ?m) (W::agr (? agr W::3s W::3p)))
   (SENSES
    ((LF ONT::ANY)
     (non-hierarchy-lf t)(TEMPL quan-count-mass-TEMPL)
     )
    )
   )
))

