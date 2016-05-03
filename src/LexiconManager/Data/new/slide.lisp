;;;;
;;;; w::slide
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::slide
	   (senses
	    ((lf-parent ont::image)
	     (Example "slide number 4 on the computer screen")
	     (templ count-pred-templ)
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("slide%1:06:01") :comments portability-experiment)
	     )	   	   	   	   
	    ))
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::slide
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("slide%2:38:00" "slide%2:38:01" "slide%2:38:02"))
     (LF-PARENT ONT::move-fluidly)
     (TEMPL affected-templ) ; like move,bounce
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("slide%2:38:00" "slide%2:38:01" "slide%2:38:02"))
     (LF-PARENT ONT::move-fluidly)
 ; like rotate,turn,spin
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("slide%2:38:00" "slide%2:38:01" "slide%2:38:02"))
     (LF-PARENT ONT::move-fluidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

