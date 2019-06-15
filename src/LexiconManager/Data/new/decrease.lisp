;;;;
;;;; w::decrease
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::decrease
  (wordfeats (W::morph (:forms (-vb) :nom W::decrease  :nomobjpreps (w::in w::of))))
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::decrease)
   (example "decrease the clock speed [to 1Ghz]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::decrease)
   (example "it decreased in temperature")
   (templ affected-templ))
   )
  )
 )
)

