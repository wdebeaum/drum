;;;;
;;;; w::maximize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::maximize
  (wordfeats (W::morph (:forms (-vb) :nom w::maximum)))
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::maximize)
   (example "maximize the similarities")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-FORMAL-XP-TEMPL)
   )
  )
 )
))

