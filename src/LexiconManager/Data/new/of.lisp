;;;;
;;;; W::of
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::of w::course)
   (SENSES
    ((LF-PARENT ONT::likelihood)
     (TEMPL PRED-S-VP-TEMPL)
     (meta-data :origin cardiac :entry-date 20090427 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (TEMPL DISC-TEMPL)
     (meta-data :origin cardiac :entry-date 20090427 :change-date nil :comments nil)
     (preference. 98)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::of
   (SENSES
    ((LF-PARENT ONT::ASSOC-WITH)
     (example "the budget of the company is 6000 dollars")
     (meta-data :origin calo :entry-date 20040901 :change-date nil :comments calo-y2)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (PREFERENCE 0.97)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::of
   (SENSES
    ((LF (W::OF))
     (non-hierarchy-lf t))
    )
   )
))

