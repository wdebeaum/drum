;;;;
;;;; w::pain
;;;;

(define-words :pos W::n
 :words (
;
;(define-list-of-words :pos W::n
;  :senses (
;   ((LF-PARENT ONT::diabetes-med)
;    (TEMPL BARE-PRED-TEMPL)
;    )
;   )
; :words (w::insulin))
;
;(define-list-of-words :pos W::n
;  :senses (
;   ((LF-PARENT ONT::diuretic)
;    (TEMPL count-PRED-TEMPL)
;    )
;   )
; :words (w::diuretic))
;
;(define-list-of-words :pos W::n
;  :senses (
;   ((LF-PARENT ONT::statin)
;    (TEMPL count-PRED-TEMPL)
;    )
;   )
; :words (w::statin))
;
;(define-list-of-words :pos W::n
;  :senses (
;   ((LF-PARENT ONT::anticoagulant)
;    (TEMPL count-PRED-TEMPL)
;    )
;   )
; :words (w::anticoagulant (w::blood w::thinner)))
  ((w::pain w::reliever)
  (senses
   ((LF-PARENT ONT::pain-reliever)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  (W::pain
  (senses
   ((meta-data :wn ("pain%1:26:00" "pain%1:09:00"))
    (LF-PARENT ONT::pain)
    (EXAMPLE "the patient developed severe pain and distension")
    (TEMPL bare-pred-TEMPL)
    )
   ((meta-data :origin domain-reorganization :entry-date 20170831 :CHANGE-date nil :comments nil :wn ("pain%1:12:00"))
    (LF-PARENT ONT::pain-scale)
    (EXAMPLE "the pain/painfulness of loneliness")
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))



#|
(define-words :pos W::n
 :words (
  (w::pain
  (senses;;;;; names of medical conditions/symptoms that are mass nouns
   ;;;;; i.e., they can't take an indefinite article (*an arthritis) and they have no plural form
   ((meta-data :wn ("pain%1:26:00" "pain%1:09:00"))
    (LF-PARENT ONT::pain)
    (TEMPL MASS-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))
|#

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::pain
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("pain%2:37:00"))
     (LF-PARENT ONT::evoke-hurt)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

