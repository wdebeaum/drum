;;;;
;;;; w::level
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::level
	   (senses
	    ;; why isn't this just ont::level? the only thing that is "mental" about it is the subcat. 
;            ((lf-parent ont::mental-object)
;             (example "His level of understanding")
;             (templ other-reln-templ)
;             (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("level%1:26:00") :comments portability-experiment)
;             (preference .97) ;; prefer ont::level sense
;             )
	    ((lf-parent ont::level)
	     (example "the level of water")
	     (TEMPL other-reln-templ)
	     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("level%1:07:00") :comments nil)
	     )	  
	    ((lf-parent ont::level)
	     (example "the humidity level" "the water level" "a level of 5")
	     (TEMPL reln-subcat-of-number-TEMPL)
	     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("level%1:07:00") :comments nil)
	     )	  
	    ))
))

