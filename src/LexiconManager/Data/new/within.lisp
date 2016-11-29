;;;;
;;;; W::WITHIN
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::WITHIN
   (SENSES
    ((LF-PARENT ONT::TIME-deadline-rel)
     (example "within two hours")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::TIME-culmination-rel)
     (example "The meeting finished within two hours")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::IN-LOC)
      (w::allow-deleted-comp +)
      (example "the dog is within the boundaries of the property")
     ;(TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    ((LF-PARENT ONT::SCALE-RELATION)
      (example "within five miles")
;     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::WITHIN
   (SENSES
    ((LF (W::WITHIN))
     (non-hierarchy-lf t))
    )
   )
))

