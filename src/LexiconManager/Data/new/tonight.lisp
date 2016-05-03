;;;;
;;;; W::TONIGHT
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::TONIGHT
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
      (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::TONIGHT)))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::TONIGHT
   (SENSES
    ((LF-PARENT ONT::DATE-OBJECT)
     (PREFERENCE 0.97)
     )
    )
   )
))

