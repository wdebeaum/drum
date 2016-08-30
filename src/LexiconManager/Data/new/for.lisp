;;;;
;;;; W::for
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::for w::what w::it w::is w::worth)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::for W::that W::matter)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::for W::starters)
   (SENSES
    ((LF-PARENT ONT::sequence-position)
     (example "for starters let's determine your price range")
     (TEMPL disc-templ)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::for w::a W::start)
   (SENSES
    ((LF-PARENT ONT::sequence-position)
     (example "for a start let's determine your price range")
     (TEMPL disc-templ)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::for w::example)
   (SENSES
    ((LF-PARENT ONT::qualification)
     (example "take this mac for example")
     (TEMPL disc-templ)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::FOR
   (SENSES
    ((LF-PARENT ONT::PURPOSE)
     (example "He runs for his health")
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    #|
     ((LF-PARENT ONT::PURPOSE)
     (example "Hit return for more results")
     (meta-data :origin plot :entry-date 20081120 :change-date nil :comments chcs-tests)
     (TEMPL BINARY-CONSTRAINT-S-obj-val-TEMPL)
     (preference .97)
     )
    |#
    
    ((LF-PARENT ONT::PURPOSE)
     (example "she is happy for him to come; switch X has to be open for bulb A to light")
     (TEMPL adv-double-subcat-control-templ)
     )
    ;;;;; Reason convers just about everything, therefore, slightly lovered priority
    ;;;;; taking aspirin for foot, for headaches
    ;; 2006/05/24 these cases covered by purpose
;    ((LF-PARENT ONT::REASON)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     (PREFERENCE 0.98)
;     )
    ((LF-PARENT ONT::ASSOC-WITH)
     (example "the company's budget for the computer purchase is 2000 dollars")
     (meta-data :origin calo :entry-date 20040901 :change-date nil :comments calo-y2)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (PREFERENCE 0.97)
     )
    ((LF-PARENT ONT::BENEFICIARY)
     (example "pick up the laundry for him")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::TIME-DURATION-REL)
     (example "he ran for five hours")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::spatial-DISTANCE-REL)
     (example "he ran for five miles")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::PURCHASE-COST)
     (meta-data :origin calo :entry-date 20040423 :change-date nil :comments calo-y1v4)
     (example "I got it for five dollars")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::COST-RELATION)
     (meta-data :origin calo :entry-date 20040423 :change-date nil :comments calo-y1v4)
     (example "I want a computer for five dollars")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::for W::now)
   (SENSES
    (;(LF-PARENT ONT::EVENT-TIME-REL)
     (LF-PARENT ONT::EVENT-TIME-REL-NOW)
     (LF-FORM W::NOW)
     (TEMPL PRED-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::FOR w::EVER)
   (SENSES
    ((lf-parent ont::time-span-rel)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (TEMPL PRED-VP-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::FOR
   (SENSES
    ((LF (W::FOR))
     (non-hierarchy-lf t))
    )
   )
))

