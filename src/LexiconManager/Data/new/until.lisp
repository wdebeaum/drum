;;;;
;;;; W::UNTIL
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::UNTIL
   (SENSES
    ; 3/2011 conflating :sit-val and :val roles
    ((LF-PARENT ONT::event-time-rel)
;     (TEMPL binary-constraint-SIT-VAL-S-decl-TEMPL)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "he was sad until he went to the party")
     )
 ; 3/2011 conflating :sit-val and :val roles
;    ((LF-PARENT ONT::event-time-rel)
;     (example "he was sad until the party")
;     (TEMPL binary-constraint-SIT-VAL-NP-TEMPL)
;     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :comments nil)
     (example "show me departures until 5 pm / the party")
     (TEMPL binary-constraint-s-or-np-TEMPL)
     )
    (;(LF-PARENT ONT::time-culmination-rel)
     (LF-PARENT ONT::event-time-rel)
     (meta-data :origin cernl :entry-date 20110105 :change-date nil :comments hpi-acn-1)
     (example "he was happy until recently" "it won't be ready until later")
     (templ binary-constraint-time-adv-result-val-templ) 
     )
    (;(LF-PARENT ONT::time-culmination-rel)
     (LF-PARENT ONT::event-time-rel)
     (meta-data :origin cernl :entry-date 20110105 :change-date nil :comments hpi-acn-1)
     (example "Beat the mixture until fluffy")
     (templ binary-constraint-adj-result-val-templ)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::UNTIL
   (SENSES
    ((LF (W::UNTIL))
     (non-hierarchy-lf t))
    )
   )
))

