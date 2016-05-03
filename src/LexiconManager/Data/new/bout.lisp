;;;;
;;;; W::BOUT
;;;;

(define-words :pos W::ADV
 :words (
  (W::BOUT
   (SENSES
    ((LF-PARENT ONT::time-clock-rel)
     (LF-FORM W::APPROXIMATE)
     (example "bout midnight")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ;; what's an example of this?
;    ((LF-PARENT ONT::APPROXIMATE-AT-LOC)
;     (LF-FORM W::APPROXIMATE)
;     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
;     (SYNTAX (W::ALLOW-DELETED-COMP +))
;     )    
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::APPROXIMATE)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

