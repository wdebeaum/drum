;;;;
;;;; w::deduct
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::deduct
 (senses
  ((meta-data :origin calo :entry-date 20050324 :change-date 20090522 :comments caloy2)
   (LF-PARENT ONT::calc-subtract)
   (example "deduct five dollars [from the price]")
   (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-FORMAL-FORMAL1-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::from))))
   )
  )
 )
))

