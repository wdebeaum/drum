;;;;
;;;; w::draw
;;;;

(define-words :pos W::V 
 
 :tags (:base500)
 :words (
	  (w::draw
	   (wordfeats (W::morph (:forms (-vb) :past W::drew :pastpart W::drawn)))
	   (senses
	    ((lf-parent ont::write)
	     (example "Draw a line / he drew a picture [for her]")
	     (templ agent-affected-create-templ )
	     (meta-data :origin bee :entry-date 20040609 :change-date 20090506 :comments portability-experiment)
	     )
	    ((LF-PARENT ONT::PULL)
	     (SEM (F::cause F::agentive) (F::aspect F::unbounded) (F::time-span F::extended))
	     (TEMPL agent-affected-xp-templ)
	     )
	    ))
))

