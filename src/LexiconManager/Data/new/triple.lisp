;;;;
;;;; W::triple
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::triple
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date nil :wn ("triple%5:00:01:multiple:00") :comments LM-vocab)
      (LF-PARENT ONT::Size-Val)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::triple
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::triple)
   (example "triple the budget [to 3K]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::triple)
   (example "it tripled (in size)")
;   (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
   (templ affected-templ)
   )
  )
 )
))
