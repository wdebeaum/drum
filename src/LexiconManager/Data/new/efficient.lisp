;;;;
;;;; W::efficient
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::efficient
     (wordfeats (W::MORPH (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060123 :change-date nil :wn ("efficient%3:00:00") :comments caloy3)
     (LF-PARENT ONT::speed-VAL)
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::efficient
	   (senses
	    ((lf-parent ont::status-val)
	     ;; Put it down as status until we can figure out better where it belongs
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :wn ("efficient%3:00:00") :comments portability-experiment)
	     )
	    ))
))

