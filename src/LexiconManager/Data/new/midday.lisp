;;;;
;;;; W::MIDDAY
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::midday
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
      (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::midday)))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::midday
   (SENSES
    ((LF-PARENT ONT::time-loc)
     (PREFERENCE 0.97)
     )
    )
   )
  ))
