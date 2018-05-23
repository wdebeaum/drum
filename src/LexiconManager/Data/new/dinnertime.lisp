;;;;
;;;; w::dinnertime
;;;;

(define-words :pos W::n
 :words (
  (w::dinnertime
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ont::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

