;;;;
;;;; w::weekday
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::weekday
   (senses
    (;(LF-PARENT ONT::time-interval)
     (LF-PARENT ONT::day)
     (SEM (F::time-function F::day-period))
    (templ time-reln-templ)
    )
   )
  ;; shouldn't eve and morn be separately identified as abbreviations?
)
))

