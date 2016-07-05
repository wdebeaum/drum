;;;;
;;;; w::consistent
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::consistent
	   (wordfeats (W::MORPH (:FORMS (-LY))))
	   (senses
	    ((lf-parent ont::consistent)
	     (meta-data :origin task-learning :entry-date 20050830 :change-date 20051028 :wn ("consistent%3:00:00") :comments nil)
	     (example "a consistent solution" "a solution consistent with the constraints")
	     (TEMPL central-adj-xp-TEMPL (XP (% W::PP (W::PTYPE W::with))))
	     )
 	    (;(lf-parent ont::continuous-val)
	     (lf-parent ont::homogeneous-val)
	     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("consistent%3:00:01") :comments nil)
	     (example "a consistent surface")
	     )
	    ))
))

