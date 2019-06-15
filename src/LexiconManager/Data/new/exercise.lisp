;;;;
;;;; w::exercise
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::exercise
	   (senses
	    ((lf-parent ont::ps-object)
	     (example "an exercise [in measuring curent]")
	     (templ other-reln-templ (xp (% w::pp (w::ptype w::in))))
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("exercise%1:04:01") :comments portability-experiment)
	     )
	    ((meta-data :origin chf :entry-date 20070809 :comments chf-dialogues :wn ("exercise%1:04:00"))
	     (LF-PARENT ONT::working-out)
	     (example "do you get enough exercise")
	     (templ mass-pred-templ)
	     )
	    ))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::exercise
   (SENSES
    ((LF-PARENT ONT::USE)
     (example "exercise that option")
     (SEM (F::ASPECT F::DYNAMIC))
     )
    ((LF-PARENT ONT::WORKING-OUT)
     (example "You need to exercise every day")
     (meta-data :origin chf :entry-date 20070809 :change-date nil :comments chf-dialogues)
     (templ agent-templ)
     )
    ((LF-PARENT ONT::WORKING-OUT)
     (example "You need to exercise your muscles every day")
     (meta-data :origin chf :entry-date 20070809 :change-date nil :comments chf-dialogues)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

