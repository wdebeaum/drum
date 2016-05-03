;;;;
;;;; w::thicken
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::thicken
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::increase)
   (example "thicken the sauce")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::increase)
   (example "it thickened with flour")
   (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt W::with)))))
   )
  )
 )
))

