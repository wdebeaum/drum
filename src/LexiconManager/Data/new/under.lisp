;;;;
;;;; W::UNDER
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::UNDER
   (SENSES
;    ((LF-PARENT ONT::VIA)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     )
    ((LF-PARENT ONT::BELOW)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )

    ((LF-PARENT ONT::situated-in)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     
     )
    
    ((LF-PARENT ONT::less-than-rel)
     (example "buy it for under five dollars")
      (TEMPL NUMBER-OPERATOR-TEMPL)
      (lf-form w::less)
      (meta-data :origin calo :entry-date 20040426 :change-date nil :comments calo-y1v1)
     )
    ((LF-PARENT ONT::less-than-rel)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "purchases under five dollars")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin calo :entry-date 20040112 :change-date nil :comments calo-y1v1)
    )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
   (W::under
   (SENSES
    ((LF (W::under))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::under-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-LOW)
    (example "underproduce; underperform; undersample")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::under-
   (SENSES
    (
     (LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (example "underexposure")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::under-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-LOW)
    (example "underexposed")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
