;;;;
;;;; W::refer
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::refer
  (wordfeats (W::morph (:forms (-vb) :past W::referred :ing W::referring :nom referral)))
  (SENSES
   ;;; Not sure what this one did that the second one doesn't do
     #|((meta-data :origin calo :entry-date 20031230 :change-date 20090506 :comments html-purchasing-corpus)
      (LF-PARENT ONT::scrutiny)
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-associated-information-TEMPL (xp (% W::pp (W::ptype W::to))))
      )|#
    ((meta-data :origin cernl :entry-date 20100503)
     (LF-PARENT ONT::SUGGEST)
     (example "I refer him (to you)")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-AGENT1-OPTIONAL-TEMPL)
     )
     )
   )
))

