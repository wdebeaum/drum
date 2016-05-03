;;;;
;;;; W::weekend
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::weekend
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("weekend%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::weekend
  (senses((LF-PARENT ONT::time-interval)
    (SEM (F::time-function F::day-period))
    (templ time-reln-templ)
    )
   )
  ;; shouldn't eve and morn be separately identified as abbreviations?
)
))

