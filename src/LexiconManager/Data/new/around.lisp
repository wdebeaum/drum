;;;;
;;;; W::AROUND
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::AROUND
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::APPROXIMATE)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::AROUND
   (SENSES
    ((LF-PARENT ONT::TIME-clock-rel)
     (LF-FORM W::APPROXIMATE)
     (example "it's around 5 pm")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ont::near-reln );ONT::proximity
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "he ran around the house")
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    ((LF-PARENT ont::near-reln );ONT::proximity)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "the house around the corner")
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
   (W::Around
   (SENSES
    ((LF (W::against))
     (non-hierarchy-lf t))
    )
   )
))

