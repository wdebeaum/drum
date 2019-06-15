;;;;
;;;; W::remind
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (W::remind
	   (SENSES
	    ((LF-PARENT ONT::TELL)
	     (example "remind me [ of the rule ]")
	     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% w::PP (w::ptype w::of))))
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     )
	    ((LF-PARENT ONT::TELL)
	     (example "remind him about the date")
	     (TEMPL AGENT-AGENT1-FORMAL-XP-PP-ABOUT-TEMPL (xp (% w::PP (w::ptype w::about))))
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     )	    
	    ((EXAMPLE "remind him that the meeting is tomorrow")
	     (LF-PARENT ONT::TELL)
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	     )
	    (;;(LF-PARENT ONT::COMMAND)
	     (lf-parent ont::remind) ;;  20120524 GUM change new parent
	     (example "remind him to do it")
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL)
	     )
	    ))
))

