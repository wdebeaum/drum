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
    (;(LF-PARENT ont::near-reln );ONT::proximity
     (LF-PARENT ont::around)
     ;(TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "he ran around the house")
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
   #|| ((LF-PARENT ont::pos-defined-by-around );ONT::proximity)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "the house around the corner")
     )||#
    ((LF-PARENT ont::pivot )
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "rotate around the origin")
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
   (W::Around
   (SENSES
    ((LF (W::around))
     (non-hierarchy-lf t))
    )
   )
))

