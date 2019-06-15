;;;;
;;;; W::catalyze
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::catalyze
    (wordfeats (W::morph (:forms (-vb) :nom w::catalysis)))
    (senses 
     ((LF-PARENT ont::catalyze)
      ;(SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))
