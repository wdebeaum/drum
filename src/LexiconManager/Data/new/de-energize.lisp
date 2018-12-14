;;;;
;;;; w::de-energize
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
	  ((w::de w::punc-minus w::energize)
           (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::de w::punc-minus w::energizes)
                                 :past (W::de w::punc-minus w::energized)
                                 :pastpart (W::soft w::punc-minus w::energized)
                                 :ing (W::de w::punc-minus w::energizing)
				 :nom (w::de w::punc-minus w::energization))))
	   (senses
	    ((LF-parent ont::change-device-state) (lf-form w::deenergize))
	    ((LF-parent ont::change-device-state) 
	     (templ agent-templ)
	     (Example "Intransitive usage: how do I deenergize "))
	    ))
))

