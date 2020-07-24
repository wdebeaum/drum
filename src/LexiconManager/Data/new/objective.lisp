;;;;
;;;; W::OBJECTIVE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::OBJECTIVE
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20050817 :wn ("objective%1:09:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::goal)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::objective
  (senses
   ((lf-parent ont::not-biased-val)
    (TEMPL central-adj-templ)
    (EXAMPLE "an objective appriasal")
    )
   )
)
))

