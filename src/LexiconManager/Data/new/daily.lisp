
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::daily
   (SENSES
    ((meta-data :origin calo :entry-date 20040504 :change-date nil :wn ("daily%3:01:00") :comments calo-y1variants)
     (lf-parent ont::specified-period-val)
     (example "they have daily meetings" "a daily vitamin")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::daily
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     )
    )
   )
))

