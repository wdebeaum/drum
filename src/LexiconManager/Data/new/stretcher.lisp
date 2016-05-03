;;;;
;;;; W::stretcher
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::stretcher
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("stretcher%1:06:00"))
     (LF-PARENT ONT::stretcher)
     (SEM (F::mobility F::non-self-moving))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::STRETCHER W::BOUND)
   (SENSES
    ((LF-PARENT ONT::BODY-PROPERTY-VAL)
     )
    )
   )
))

