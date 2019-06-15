;;;;
;;;; W::kick
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::kick
    (wordfeats (W::morph (:forms (-vb) :nom w::kick)))
    (SENSES
     ((LF-PARENT ONT::kicking)
      (example "he kicked the wall")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     ))
))

