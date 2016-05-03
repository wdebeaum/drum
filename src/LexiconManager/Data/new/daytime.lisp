;;;;
;;;; w::daytime
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::daytime
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::time-interval)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

