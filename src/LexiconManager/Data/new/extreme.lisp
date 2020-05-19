;;;;
;;;; W::extreme
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::extreme
   (SENSES
    ((lf-parent ont::severe-val)
     (example "under extreme conditions")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :wn ("extreme%5:00:00:intense:00") :comments caloy2)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::extreme
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("end%1:15:00"))
     ;(LF-PARENT ont::pos-end-of-trajectory);ONT::LINE-DEPENDENT-LOCATION)
     (LF-PARENT ONT::END-LOCATION)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
)))
