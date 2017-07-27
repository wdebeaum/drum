;;;;
;;;; W::FEW
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::FEW
   (wordfeats (W::status W::indefinite-plural))
   (SENSES
    ((LF ONT::FEW)
     (non-hierarchy-lf t)(TEMPL quan-3p-templ)
     )
    )
   )
))

;(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
; :words (
;  (W::few
;   (SENSES
;    ((LF-PARENT ONT::inadequate)
;     (meta-data :origin adjective-reorganization :entry-date 20170413 :change-date nil)
;     (EXAMPLE "honest men are few")
;     )
;    )
;   )
;))