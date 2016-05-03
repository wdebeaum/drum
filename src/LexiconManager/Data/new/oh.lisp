;;;;
;;;; W::OH
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
   ;; MD 2008/06/09 FIXME -- should this really be disc-templ? Smth else?
  (W::OH
   (SENSES
    ((LF-PARENT ONT::SpeakerStatus)
     (TEMPL disc-templ)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::OH
   (SENSES
    ((LF (W::oh))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_ACK))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::oh W::my W::goodness)
   (SENSES
    ((LF (W::OH-MY-GOODNESS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     (preference .97) 
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::oh W::my W::god)
   (SENSES
    ((LF (W::OH-MY-GOD))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     (preference .97) 
     )
    )
   )
))

