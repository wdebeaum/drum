(define-words :pos W::ADV
 :tags (:base500)
 :words (
	 (W::upon
	  (SENSES
	   #||((LF-PARENT ONT::event-event-time)  ;; if treat this example as a gerund, we only need the one sense
	    (meta-data :origin calo :entry-date 20040809 :change-date nil :comments caloy2)
	    (example "I felt sick upon arriving")
	    (TEMPL binary-constraint-S-ing-TEMPL)
	    )||#
	   ((LF-PARENT ONT::event-event-time)
	    (meta-data :origin calo :entry-date 20040809 :change-date nil :comments caloy2)
	    (example "upon inspection/loading we found the flaw")
;	    (TEMPL binary-constraint-s-TEMPL)
	    (TEMPL binary-constraint-S-OR-NP-templ)
	    )
	   )
	  )
 ))


(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::UPON
   (SENSES
    ((LF (W::UPON))
     (non-hierarchy-lf t))
    )
   )
))
