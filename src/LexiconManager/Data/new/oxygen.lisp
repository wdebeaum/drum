;;;;
;;;; W::OXYGEN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::OXYGEN
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("oxygen%1:27:00"))
     (LF-PARENT ONT::substance)
     (SEM (F::form F::gas))
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::oxygen
  (senses
   ((LF-PARENT ONT::natural-gas-substance)
    (TEMPL mass-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    )
   )
)
))

