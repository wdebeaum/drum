;;;;
;;;; W::AGAINST
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
   (W::AGAINST
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "he ran against the wind")
     )
     ((LF-PARENT ONT::CONTRA-FORCE)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "he fought against the proposal/enemy; he pushed against the wall")
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
   (W::Against
   (SENSES
    ((LF (W::against))
     (non-hierarchy-lf t))
    )
   )
))

