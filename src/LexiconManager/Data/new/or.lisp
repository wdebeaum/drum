;;;;
;;;; W::or
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::or W::so)
   (SENSES
    ((LF-PARENT ONT::precision-val)
     (LF-FORM W::approximate)
     (TEMPL binary-constraint-measure-NP-templ)
     )
    ((LF-PARENT ONT::precision-val)
     (LF-FORM W::approximate)
     (TEMPL number-operator-post-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::OR
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::OR
   (SENSES
    ((LF W::OR)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-DISJ-TEMPL)
     (SYNTAX (w::seq +) (status w::indefinite))
     )
    )
   )
))

