;;;;
;;;; W::dysregulate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::dysregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::dysregulation)))
    (senses 
     ((LF-PARENT ont::HINDERING)
      ;(SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))
