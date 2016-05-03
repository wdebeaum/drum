;;;;
;;;; W::ANYTIME
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
   (W::ANYTIME
   (wordfeats (W::ATYPE (? atype W::pre W::post)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::ANYTIME)))
     (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     )
    )
   )
))

