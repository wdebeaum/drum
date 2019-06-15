;;;;
;;;; w::fluctuate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::fluctuate
   (wordfeats (W::morph (:forms (-vb) :nom w::fluctuation)))
 (senses
  ((meta-data :origin plow :entry-date 20060802 :change-date 20090504 :comments weather)
   (LF-PARENT ONT::fluctuate)
   (example "fluctuation in the atmospheric pressure")
   )
  ((meta-data :origin step :entry-date 20080603 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::fluctuate)
   (example "research and development fluctuated (with the budget)")
   (TEMPL AFFECTED-FORMAL-XP-OPTIONAL-TEMPL  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
   )
  )
 )
))

