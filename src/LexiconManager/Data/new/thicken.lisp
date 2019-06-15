;;;;
;;;; w::thicken
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::thicken
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   ;(LF-PARENT ONT::increase)
   (LF-PARENT ONT::thicken)
   (example "thicken the sauce")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::texture-thickness-scale))
    )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   ;(LF-PARENT ONT::increase)
   (LF-PARENT ONT::thicken)
   (example "it thickened with flour")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::texture-thickness-scale))
   (templ affected-unaccusative-templ)
   )
  )
 )
))

