;;;;
;;;; w::procedure
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;    ))
  (w::procedure
   (senses
    #|
    ((lf-parent ont::action)
     (example "execute the procedure")
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil :wn ("procedure%1:04:00" "procedure%1:04:02" "procedure%1:04:01"))
     )
    |#
     ((lf-parent ont::procedure)
      (templ other-reln-templ)
      (example "the procedure for solving this problem")
      (meta-data :origin calo :entry-date 20040621 :change-date 20050817 :wn ("procedure%1:04:00") :comments lf-restructuring)
      )
    ))
))

