;;;;
;;;; w::de-energise
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  ((w::de w::punc-minus w::energise)
           (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::de w::punc-minus w::energises)
                                 :past (W::de w::punc-minus w::energised)
                                 :pastpart (W::soft w::punc-minus w::energised)
                                 :ing (W::de w::punc-minus w::energising)
                                 :nom (w::de w::punc-minus w::energisation))))
	   (senses
	    ((LF-parent ont::change-device-state) (lf-form w::deenergize))
	    ((LF-parent ont::change-device-state) 
	     (templ agent-templ)
	     (Example "Intransitive usage: how do I deenergize "))
	    ))
))

