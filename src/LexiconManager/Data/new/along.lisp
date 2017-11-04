;;;;
;;;; W::ALONG
;;;;

(define-words :pos W::ADV
 :words (
  (W::ALONG
   (SENSES
    ((LF-PARENT ONT::ALONG)
     (example "he traveled along the river")
     ;(TEMPL BINARY-CONSTRAINT-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    ((LF-PARENT ONT::linear-extent)
     ;(TEMPL BINARY-CONSTRAINT-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "he found it along the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     ;(preference .97) ;; prefer trajectory sense
     )
    )
   )
))

(define-words :pos W::conj 
 :words (
  ((W::ALONG W::WITH)
   (wordfeats (W::conj +) (W::seq +))
   (SENSES
    ((LF ONT::AND)
     (non-hierarchy-lf t) (TEMPL SUBCAT-ANY-TEMPL)
;     (syntax (w::status w::definite-plural))
     (syntax (w::status (? s ont::definite-plural ont::indefinite-plural)))
     )
    )
   )
))
