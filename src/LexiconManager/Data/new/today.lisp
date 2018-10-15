;;;;
;;;; W::TODAY
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::TODAY
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS ONT::TODAY)))
     )
    )
   )
)

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::TODAY
   (SENSES
    ((LF-PARENT ONT::time-lOC)
     (PREFERENCE 0.97)
     )
    )
   )
))

