;;;;
;;;; W::YESTERDAY
;;;;

#|
(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::YESTERDAY
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS ONT::YESTERDAY))
     )
    )
   )
))
|#

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::YESTERDAY
   (SENSES
    (;(LF-PARENT ONT::TIME-LOC)
     (LF-PARENT ONT::YESTERDAY)
     ;(PREFERENCE 0.97)
     )
    )
   )
))

