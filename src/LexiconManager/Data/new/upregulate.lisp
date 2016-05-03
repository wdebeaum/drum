;;;;
;;;; W::upregulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::upregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::upregulation)))
    (senses 
     ((LF-PARENT ont::cause-stimulate)
;      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((W::up W::regulate)
    (wordfeats (W::morph (:forms (-vb)
				 :3s (W::up W::regulates)
				 :past (W::up W::regulated)
				 :pastpart (W::up W::regulated)
				 :ing (W::up W::regulating)
				 :nom (w::up w::regulation))))
    (senses 
     ((LF-PARENT ont::cause-stimulate)
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((W::up W::punc-minus W::regulate)
    (wordfeats (W::morph (:forms (-vb)
				 :3s (W::up W::punc-minus W::regulates)
				 :past (W::up W::punc-minus W::regulated)
				 :pastpart (W::up W::punc-minus W::regulated)
				 :ing (W::up W::punc-minus W::regulating)
				 :nom (w::up W::punc-minus w::regulation))))
    (senses 
     ((LF-PARENT ont::cause-stimulate)
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))
