;;;;
;;;; W::DAY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::DAY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("day%1:28:00" "day%1:28:06"))
     (LF-PARENT ONT::day-duration)
     (example "day of 24 hours" "for some days" "a 5-day week")
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::day)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((w::day w::of w::the w::dead)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

