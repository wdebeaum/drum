
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::weekly
   (SENSES
    ((meta-data :origin calo :entry-date 20040504 :change-date nil :wn ("weekly%3:01:00") :comments calo-y1variants)
     (lf-parent ont::specified-period-val)
     (example "they have weekly meetings")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   (W::weekly
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     )
    )
   )
))

