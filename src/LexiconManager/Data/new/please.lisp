;;;;
;;;; W::please
;;;;

(define-words :pos W::V 
 :words (
  (W::please
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("please%2:37:00"))
     (LF-PARENT ONT::evoke-joy)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::PLEASE
   (SENSES
    ((LF-PARENT ONT::POLITENESS)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::PLEASE
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

