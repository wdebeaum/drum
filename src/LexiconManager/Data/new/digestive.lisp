;;;;
;;;; w::digestive
;;;;

(define-words :pos W::n
 :words (
;; internal
  ((w::digestive w::system)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
;; internal
  ((w::digestive w::tract)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::adj
 :words (
  (w::digestive
  (senses
   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

