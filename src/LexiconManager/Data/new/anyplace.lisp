;;;;
;;;; W::ANYPLACE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::ANYPLACE
   (wordfeats (W::else-word +))
   (SENSES
    ((LF-PARENT ONT::WH-LOCATION)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     )
    )
   )
))

