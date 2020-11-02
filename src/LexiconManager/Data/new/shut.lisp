;;;;
;;;; W::shut
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::shut (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::shut)))
   (SENSES
    ((LF-PARENT ONT::deactivate-turn-off) ;turn-off)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "shut off the power")
     )
    )
   )
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
  :words (
	  (w::shut
	   (senses
	    ((LF-parent ont::close)
	     (example "close the door")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     )
	    ((lf-parent ont::close)
	     (templ affected-templ)
	     (example "after the piston moves upward the valve closes")
	     (meta-data :origin mobius :entry-date 20080702 :change-date 20091008 :comments engine-text01)
	     )
	    ))
	  ))
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::shut
   (SENSES
    ((meta-data :origin adjective-reorganization :entry-date 20170403 :change-date nil :comments nil :wn nil :comlex nil)
     (lf-parent ont::inactive-closed)
     )
    )
   )))
