;;;;
;;;; w::correspond
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (w::correspond
	   (senses
	    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("battle-36.4-1"))
	     (LF-PARENT ONT::talk)
	     (TEMPL agent-about-theme-addressee-optional-templ) ; like communicate
	     (PREFERENCE 0.96)
	     )
	    ((lf-parent ont::in-relation)
	     (templ neutral-neutral-templ (xp (% w::pp (w::ptype (? pptp w::with w::to)))))
	     (Example "this corresponds to ...")
	     (meta-data :origin bee :entry-date 20040609 :change-date 20040805 :comments portability-experiment)
	     )
	    ))
))

