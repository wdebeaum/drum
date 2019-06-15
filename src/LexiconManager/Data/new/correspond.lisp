;;;;
;;;; w::correspond
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::correspond
	   (senses
	    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("battle-36.4-1"))
	     (LF-PARENT ONT::talk)
	     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL) ; like communicate
	     (PREFERENCE 0.96)
	     )
	    ((lf-parent ont::in-relation)
	     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% w::pp (w::ptype (? pptp w::with w::to)))))
	     (Example "this corresponds to ...")
	     (meta-data :origin bee :entry-date 20040609 :change-date 20040805 :comments portability-experiment)
	     )
	    ))
))

