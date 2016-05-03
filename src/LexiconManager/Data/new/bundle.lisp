;;;;
;;;; W::bundle
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::bundle
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090520 :comments html-purchasing-corpus)
     (LF-PARENT ONT::quantity)
     (TEMPL classifier-count-pl-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::bundle
   (SENSES
    ((LF-PARENT ont::joining)
     (example "bundle the gear together")
     (meta-data :origin calo :entry-date 20040921 :change-date nil :comments caloy2)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ((EXAMPLE "bundle the red one with those orange ones")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-affected2-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    )
   )
))

