;;;;
;;;; W::FACT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::FACT
   (SENSES
    ((LF-PARENT ONT::FACT)
     (example "the fact that he left")
     (templ count-subcat-that-optional-templ)
     (meta-data :origin trips :entry-date unknown :change-date 20041201 :wn ("fact%1:09:01") :comments caloy2)
     )
    )
   )
))

