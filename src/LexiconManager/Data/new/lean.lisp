;;;;
;;;; w::lean
;;;; 

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::lean
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (LF-PARENT ONT::leaning)
     (TEMPL neutral-templ) ; like hang,stand,sit
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50") :wn ("lean%2:38:00"))
     (LF-PARENT ONT::body-movement-place)
     (TEMPL agent-templ) ; like (lie
     )
    ((EXAMPLE "lean the box against the desk" )
     (lf-parent ont::place-in-position) ;; 20120523 GUM change new parent
      (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )

    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (W::LEAN
	   (SENSES
	    (
      (lf-parent ont::lean-val)
	     (meta-data :origin foodkb :entry-date 20050811 :change-date nil :wn ("lean%3:00:04") :comments nil)
	     (example "he only eats lean meat")
	     (SEM (F::GRADABILITY F::+))
	     )
	    ))
))

