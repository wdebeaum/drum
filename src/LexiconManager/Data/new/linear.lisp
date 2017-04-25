;;;;
;;;; w::linear
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::linear
   (SENSES
    ((meta-data :origin calo :entry-date 20041122 :change-date 20090731 :wn ("linear%3:00:03") :comments caloy2)
     (lf-parent ont::analog) ;; like analog
     (example "a linear amplifier")
     (SEM (F::GRADABILITY F::-))
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::linear
	   (senses
     ((lf-parent ont::numerical-property-val)
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :wn ("linear%3:00:02") :comments lam-initial)
	     (example "linear equation")
	     )
	    ))
))

