;;;;
;;;; w::urinary
;;;;

(define-words :pos W::n
 :words (
;; internal
  ((w::urinary w::tract)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::adj
 :words (
  (w::urinary
  (senses
   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

