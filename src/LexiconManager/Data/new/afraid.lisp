;;;;
;;;; w::afraid
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::afraid
	   (senses
	    ((lf-parent ont::afraid)
	     (meta-data :origin leam :entry-date 20050421 :change-date 20070214 :wn ("afraid%3:00:00") :comments lam-initial)
	     (example "I am afraid")
	     (templ predicative-only-experiencer-adj-templ)
	     )
	    ((lf-parent ont::afraid)
	     (meta-data :origin leam :entry-date 20050421 :change-date 20070214 :wn ("afraid%3:00:00") :comments lam-initial)
	     (example "I am afraid of dogs")
	     (templ adj-stimulus-xp-templ (xp (% w::pp (w::ptype w::of))))
	     )
	    ((lf-parent ont::afraid)
	     (meta-data :origin leam :entry-date 20050421 :change-date 20070214 :wn ("afraid%3:00:00") :comments lam-initial)
	     (example "I am afraid for her")
	     (templ adj-theme-xp-templ (xp (% w::pp (w::ptype w::for))))
	     )
	    ((LF-PARENT ONT::apologize)
	     (example "I am afraid [that I don't understand]")
	     (TEMPL ADJ-THEME-XP-TEMPL (XP (% W::cp (W::ctype (? ct W::s-finite w::s-to)))))
	     (meta-data :origin leam :entry-date 20050421 :change-date 20061107 :wn ("afraid%5:00:00:concerned:00") :comments lam-initial)
	     )	    
	    )	   
	   )
))

