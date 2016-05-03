;;;;
;;;; W::broadband
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
      (W::broadband
    (wordfeats (W::morph (:forms (-none))))
    (SENSES
     ((EXAMPLE "I want a wireless broadband router")
      (LF-PARENT ONT::bandwidth-VAL) ;; need a more specific lf, but I don't know what it should be. Try to get more exemplars
      (SEM (F::GRADABILITY F::-))
      (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
      )
     )
    )
))

