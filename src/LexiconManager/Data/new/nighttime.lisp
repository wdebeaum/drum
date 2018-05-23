;;;;
;;;; w::nighttime
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::nighttime
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::day-stage)
    (SEM (F::time-function F::day-period ))
    (templ time-reln-templ)
     )
   )
)
))

