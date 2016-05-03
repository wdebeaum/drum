;;;;
;;;; W::PAST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PAST
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("past%1:28:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::time-span)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::PAST
   (SENSES
    ((LF-PARENT ONT::pos-after-in-trajectory)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

