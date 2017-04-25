;;;;
;;;; w::well
;;;; 

(define-words :pos W::adj 
 :tags (:base500)
 :words (
  (W::WELL
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("well%3:00:01"))
     (lf-parent ont::healthy-val)
     (example "He is finally well")
     (preference .98)
     (templ predicative-only-adj-templ)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
     ;; MD 2008/06/09 FIXME -- should this really be disc-templ? Smth else?
  (W::WELL
   (SENSES
    ((LF-PARENT ONT::SpeakerStatus)
     (TEMPL disc-templ)
     (preference .97)
     )
    ((LF-PARENT ONT::manner)
     (TEMPL PRED-S-POST-TEMPL)
     (example "are you breathing well today")
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil :wn nil)
     )
     ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
      (LF-PARENT ONT::degree-modifier-high)
      (example "development was well underway")
      (templ adj-operator-templ)
            )
     )   
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::well W::done)
   (SENSES
    ((LF (W::OK))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CONFIRM))
     )
    )
   )
))

