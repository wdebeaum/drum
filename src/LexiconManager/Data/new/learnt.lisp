;;;;
;;;; w::learnt
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  ;; an exceptional form for learned
	  (w::learnt
	   (wordfeats (W::morph (:forms NIL)) (W::vform (? vfrm w::past W::pastpart)))
	   (senses
	    ((lf-parent ont::determine)
	     (Example "I learnt that voltage is a difference in charge")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-that))))
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial)
	     )
	    ((lf-parent ONT::learn)
	     (Example "I have learnt French")
	     (TEMPL AGENT-FORMAL-XP-TEMPL)
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial  :wn ("learn%2:31:00"))
	     )	    
	    ((lf-parent ONT::learn)
	     (Example "I learnt about difference in potentials [from Mary]")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::about))))
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial  :wn ("learn%2:31:00"))
	     )	    
	    ((LF-PARENT ONT::learn)
	     (example "I learnt how to do it")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::NP (W::sort W::wh-desc))))
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial  :wn ("learn%2:31:00"))
	     )
	    ))
))

