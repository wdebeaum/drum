;;;;
;;;; W::BEFORE
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::BEFORE
   (SENSES
    ((LF-PARENT ONT::before)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "he arrived before she left")
     )
    ((LF-PARENT ONT::before)
     (TEMPL binary-constraint-S-implicit-TEMPL)
     (example "he arrived before (5 pm)")
     )
    ((LF-PARENT ONT::before)
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :comments nil)
     (example "show me departures before 5 pm")
     (TEMPL binary-constraint-np-TEMPL)
     )
  
    ((LF-PARENT ONT::pos-before-in-trajectory)
     (meta-data :origin beetle :entry-date 20090406 :change-date nil :comments nil)
     (example "just before the bridge, turn left" "the bulbs that come before the switch affect it")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (preference 0.96)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
	      :words (
		      (W::before
		       (SENSES
			((LF (W::BEFORE))
			 (non-hierarchy-lf t))
			)
		       )
		      ))
