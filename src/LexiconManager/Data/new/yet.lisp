;;;;
;;;; W::yet
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::yet
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (example "add the best answer yet to the list of choices")
     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::YET)))
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::YET
   (SENSES
    ((LF-PARENT ONT::time-rel-so-far)
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::time-rel-so-far)
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

