;;;;
;;;; W::PERCH
;;;;

(define-words :pos W::n
 :words (
  (W::PERCH
  (senses
	   ((LF-PARENT ONT::FRESHWATER-fish)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::perch
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL neutral-templ) ; like hang,stand,sit
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50") :wn ("perch%2:35:00"))
     (LF-PARENT ONT::body-movement-place)
     (TEMPL agent-templ) ; like (lie
     )
    ((EXAMPLE "perch the box on the desk" )
     ;;(LF-PARENT ONT::BODY-MOVEMENT)
     (lf-parent ont::put ) ;; 20120523 GUM change new parent
      (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )

    )
   )
))

