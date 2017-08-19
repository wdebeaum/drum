;;;;
;;;; W::difference
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::difference
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("difference%1:07:00"))
     (EXAMPLE "It makes no difference")
     (LF-PARENT ONT::different-scale)
     )
    )
   )
))

#|
(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::difference
	   (senses
	    ((lf-parent ont::comparison-function)
	     (example "the difference in states between the terminals")
	     (templ reln-scale-neutral-templ (xp2 (% w::PP (w::ptype (? ptp w::of w::in)))) (xp1 (% w::PP (w::ptype w::between))))
	     (meta-data :origin bee :entry-date 20040407 :change-date nil :wn ("difference%1:07:00") :comments test-s)
	     )
	    ((lf-parent ont::comparison-function)
	     (example "the difference between states of the terminals")
	     (templ other-reln-neutral-templ (xp (% w::PP (w::ptype w::between))))
	     (meta-data :origin bee :entry-date 20081211 :change-date nil :wn ("difference%1:07:00") :comments beetle-pilots)
	     )
	    ((lf-parent ont::comparison-function)
	     (example "the difference between states between the terminals")
	     (templ reln-scale-neutral-templ (xp1 (% w::PP (w::ptype w::between)))
		    (xp2 (% w::PP (w::ptype w::between))) )
	     (meta-data :origin bee :entry-date 20090116 :change-date nil :wn ("difference%1:07:00") :comments beetle-pilots)
	     (preference 0.98) ;; MD -- put a lower preference since it's a less likely interpretation. But it cannot be lowered further, or the "spatial-loc" sense of "between" takes over if it is in-domain	     
	     )	    
	    ((lf-parent ont::comparison-function)
	     (example "the difference of 1.5V in/between the terminals")
	     (templ reln-scale-neutral-templ 
		    (xp1 (% w::PP (w::ptype (? xx w::in w::between)))))
	     (meta-data :origin bee :entry-date 20090512 :change-date nil :wn ("difference%1:07:00") :comments beetle-pilots)
	     )	    
	   #|| ((lf-parent ont::comparison-function)
	     (example "the difference of 1.5V between their states")
	     (templ reln-val-neutral-templ 
		    (xp1 (% w::PP (w::ptype w::between))) )
	     (meta-data :origin bee :entry-date 20090512 :change-date nil :wn ("difference%1:07:00") :comments beetle-pilots)
	     )||#	    	    
	    ))
))
|#
