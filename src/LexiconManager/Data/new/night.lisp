;;;;
;;;; W::night
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::night
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("night%1:28:02"))
     (LF-PARENT ONT::time-unit)
     (example "for some nights" "a 5-night stay")
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("night%1:28:00" "night%1:28:01" "night%1:28:04" "night%1:28:05" "night%1:28:03"))
     (LF-PARENT ONT::recurring-time-of-day)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::night
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::time-interval)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

