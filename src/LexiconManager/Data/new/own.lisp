;;;;
;;;; W::own
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
;   )
  (W::own
   (SENSES
    ((EXAMPLE "he owns a truck")
     (LF-PARENT ONT::possess)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OWN
   (SENSES
    ((meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil :wn nil)
     (LF-PARENT ONT::own)
     (example "his own truck")
     (TEMPL own-TEMPL)
     )
    )
   )
))

