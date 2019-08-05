;;;;
;;;; W::add
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
(W::add
 (wordfeats (W::morph (:forms (-vb) :nom w::addition)))
 (SENSES
  ((LF-PARENT ont::add-include)
   (example "add a wireless card to the order" "add the oranges into the cart")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   ;;(TEMPL agent-affected-GOAL-optional-new-TEMPL)
   )
  ((meta-data :origin calo :entry-date 20050324 :change-date 20090522 :comments caloy2)
   (LF-PARENT ONT::calc-add)
   (example "add five dollars to the price" "add 7 and 8" "add the numbers together")
   (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-FORMAL-FORMAL1-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   (preference .98)
   )
  )
 )
))

