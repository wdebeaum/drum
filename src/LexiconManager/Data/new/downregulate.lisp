;;;;
;;;; W::downregulate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::downregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::downregulation :agentnom w::downregulator)))
    (senses 
     ((LF-PARENT ont::downregulate)
      ;(SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ((W::down W::regulate)
    (wordfeats (W::morph (:forms (-vb)
				 :3s (W::down W::regulates)
				 :past (W::down W::regulated)
				 :pastpart (W::down W::regulated)
				 :ing (W::down W::regulating)
				 :nom (w::down w::regulation))))
    (senses 
     ((LF-PARENT ont::downregulate)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ((W::down W::punc-minus W::regulate)
    (wordfeats (W::morph (:forms (-vb)
				 :3s (W::down W::punc-minus W::regulates)
				 :past (W::down W::punc-minus W::regulated)
				 :pastpart (W::down W::punc-minus W::regulated)
				 :ing (W::down W::punc-minus W::regulating)
				 :nom (w::down W::punc-minus w::regulation))))
    (senses 
     ((LF-PARENT ont::downregulate)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))
