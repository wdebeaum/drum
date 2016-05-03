;;;;
;;;; W::still
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::still
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("still%5:00:01:nonmoving:00") :comments caloy3)
     (LF-PARENT ONT::motion-VAL)
     (example "a still image")
     )
    ((meta-data :origin ?? :entry-date 20080925 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::QUIET)
     (example "a still night")
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::STILL
   (SENSES
    ((LF-PARENT ONT::time-rel-so-far)
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::time-rel-so-far)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((lf-parent ont::time-rel-so-far)
     (example "still 1.5 volts")
     (meta-data :origin beetle :entry-date 20080711 :change-date nil :comments nil) 
     (templ disc-templ)
     (preference 0.92)
     )
    ))
))

