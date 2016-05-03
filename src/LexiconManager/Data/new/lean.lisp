;;;;
;;;; W::lean
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::lean
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (LF-PARENT ONT::lean)
     (TEMPL affected-templ) ; like hang,stand,sit
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50") :wn ("lean%2:38:00"))
     (LF-PARENT ONT::body-movement-place)
     (TEMPL agent-templ) ; like (lie
     )
    ((EXAMPLE "lean the box against the desk" )
     ;;(LF-PARENT ONT::BODY-MOVEMENT)
     (lf-parent ont::put) ;; 20120523 GUM change new parent
      (TEMPL AGENT-AFFECTED-goal-TEMPL)
     )

    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  ;; additions for food-kb
	  (W::LEAN
	   (SENSES
	    (
	     (LF-PARENT ONT::fat-content)
	     (meta-data :origin foodkb :entry-date 20050811 :change-date nil :wn ("lean%3:00:04") :comments nil)
	     (example "he only eats lean meat")
	     (SEM (F::GRADABILITY F::+))
	     )
	    ))
))

