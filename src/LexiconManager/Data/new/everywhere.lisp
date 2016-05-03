;;;;
;;;; W::EVERYWHERE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::EVERYWHERE
   (wordfeats (W::else-word +))
   (SENSES
    ((LF-PARENT ONT::WH-LOCATION)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::EVERYWHERE
   (wordfeats (W::else-word +))
   (SENSES
    ((LF-PARENT ONT::WH-LOCATION)
     )
    )
   )
))

