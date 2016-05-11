;;;;
;;;; W::immuno
;;;;

(define-words :pos w::adv
 :words (
  (w::immuno-
  (senses
   ((lf-parent ont::BODY-SYSTEM-VAL)
    (example "This molecule immunoprecipitated with that protein.")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::immuno-
   (SENSES
    (
     (LF-PARENT ONT::BODY-SYSTEM-VAL)
     (example "The immunoprecipitate of this protein")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))
