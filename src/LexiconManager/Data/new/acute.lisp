;;;;
;;;; W::acute
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; severity and also frequency/suddenness; want to contract with severe, and also with chronic, which for now is a continuous-val
  (W::acute
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("severe%5:00:00:intense:00"))
     (LF-PARENT ONT::SEVERITY-VAL)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

