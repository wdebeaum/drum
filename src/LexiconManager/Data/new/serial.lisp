;;;;
;;;; w::serial
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::serial
	   (senses
; I think this is covered by ONT::ordered-val, below -- wdebeaum
;	    ((lf-parent ont::physical-discrete-property-val)	     
;	     ;; Put it down as physical-discrete-property until we can figure out better where it belongs
;	     (meta-data :origin bee :entry-date 20040608 :change-date nil :wn ("serial%5:00:00:ordered:00") :comments portability-experiment)
;	     )
     ((lf-parent ont::sequential-val) ;; like consecutive
	     (example "a serial broadcast")
	     (meta-data :origin step :entry-date 20080925 :change-date nil :wn ("serial%5:00:00:ordered:00") :comments nil)
	     )
	    ))
))

