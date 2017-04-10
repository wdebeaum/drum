
(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::poorly
   (SENSES
    ((lf-parent ont::bad)
    (TEMPL ADJ-OPERATOR-TEMPL)
    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
    (example "his asthma is poorly controlled")
    (meta-data :origin asma :entry-date 20120305 :change-date nil :comments nil)
    )
   ((lf-parent ont::bad)
    (TEMPL PRED-VP-TEMPL)
    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
    (example "his ankles swe badly")
    (meta-data :origin asma :entry-date 20120305 :change-date nil :comments nil :wn nil)
    )
   )
)
))

