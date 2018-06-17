;;;;
;;;; w::mostly
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (w::mostly
   (SENSES
    ((LF-PARENT ONT::DEGREE-MODIFIER)
     (TEMPL ADJ-OPERATOR-TEMPL)
     (meta-data :origin plow :entry-date 20060615 :change-date nil :comments pq)
     (example "it is mostly red")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::mostly
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (example "the department mostly buys linux boxes")
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    ((LF-PARENT ONT::FREQUENCY)
     (example "the department mostly buys linux boxes")
     (TEMPL Post-adv-TEMPL)
     )
    ((LF-PARENT ONT::FREQUENCY)
     (example "mostly the department buys linux boxes")
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

