;;;;
;;;; w::daytime
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::daytime
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::day-stage)
    (SEM (F::time-function (? tf F::day-period f::day-period)))
    (templ time-reln-templ)
     )
   )
)
))

