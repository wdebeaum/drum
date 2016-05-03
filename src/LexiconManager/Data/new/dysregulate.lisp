;;;;
;;;; W::dysregulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::dysregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::dysregulation)))
    (senses 
     ((LF-PARENT ont::HINDERING)
;      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))
