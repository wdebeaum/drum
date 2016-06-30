;;;;
;;;; W::N^T
;;;;

(define-words :pos W::neg
 :tags (:base500)
 :words (
;; VP negation e.g. he isn't going to the party
  (W::N^T
  (senses((LF ONT::NEG)
    (non-hierarchy-lf t)(TEMPL no-features-templ)
    )
   )
)
))

;; this is needed for construction like "he isn't happy"
(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::N^T
   (SENSES
    ((LF-PARENT ONT::NEG)
     (example "not too bad")
     (TEMPL NEG-ADJ-ADV-OPERATOR-TEMPL)
     )    
    )
   )
))
