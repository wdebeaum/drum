;;;;
;;;; W::yearly
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::yearly
   (SENSES
    ((meta-data :origin calo :entry-date 20040504 :change-date nil :wn ("yearly%3:01:00") :comments calo-y1variants)
     (lf-parent ont::specified-period-val)
     (example "they have yearly meetings")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   (W::yearly
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     )
    )
   )
))

