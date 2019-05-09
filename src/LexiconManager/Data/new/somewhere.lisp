;;;;
;;;; W::SOMEWHERE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::SOMEWHERE
   (wordfeats (W::else-word +))
   (SENSES
    (;(LF-PARENT ONT::WH-LOCATION)
     (LF-PARENT ONT::AT-LOC)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::SOMEWHERE
   (wordfeats (W::ELSE-WORD +))
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (PREFERENCE 0.97)
     )
    )
   )
))

