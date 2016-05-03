;;;;
;;;; W::BUILDING
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BUILDING
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("building%1:06:00"))
     (LF-PARENT ONT::general-structure)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::building w::block)
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL count-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    )
   )
)
))

