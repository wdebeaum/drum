;;;;
;;;; w::increase
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
	      :words (
		      (w::increase
		       (wordfeats (W::morph (:forms (-vb) :nom w::increase :nomobjpreps (w::in w::of))))
		       ; the increase in temperature; the increase of bees
		       (senses
			((LF-PARENT ONT::increase)
			 (example "increase the budget [to 3K]")
			 (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
			 ;;(TEMPL AGENT-AFFECTED-scale-XP-optional-TEMPL (xp (% W::PP (W::ptype (? pt w::in W::with)))))
			 (TEMPL AGENT-AFFECTED-TEMPL)
			 (preference .98)
			 )
			((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
			 (LF-PARENT ONT::increase)
			 (example "it increased in temperature")
			 (templ AFFECTED-TEMPL)
			 ;;(templ affected-scale-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
			 )
			;; the rest are redundant with the first cases
  #|| 
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::increase)
   (example "it increased the pressure")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-xp-templ)
   )
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::increase)
   (example "it increased the pressure to 32 lbs per ")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-result-templ (xp (% W::PP (W::ptype W::to))))
   )||#
  )
 )
))

