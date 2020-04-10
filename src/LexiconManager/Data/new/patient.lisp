;;;;
;;;; W::PATIENT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PATIENT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("patient%1:18:00"))
     (LF-PARENT ONT::patient)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::PATIENT
    (wordfeats (W::MORPH (:FORMS (-LY))))
    (SENSES
     ((LF-PARENT ONT::PATIENT-val)
      (EXAMPLE "he is very patient")
      )
     )
    )
))

