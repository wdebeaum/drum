;;;;
;;;; W::SOMETIME
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::SOMETIME
   (wordfeats (W::ATYPE (? atype W::pre W::post)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::SOMETIME)))
     )
    )
   )
))

