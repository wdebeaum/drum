;;;;
;;;; w::impact
;;;;

(define-words :pos W::V 
 :words (
	  (w::impact
	   (wordfeats (W::morph (:forms (-vb) :nom W::impact :nomsubjpreps (w::of) :nomobjpreps (w::on))))
	   (senses
	    ((lf-parent ont::objective-influence)
	     (example "the switch impacts the state of the lightbulb")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )
	    ))
))

