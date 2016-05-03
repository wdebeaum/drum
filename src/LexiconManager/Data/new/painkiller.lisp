;;;;
;;;; w::painkiller
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
  (w::painkiller
  (senses
   ((LF-PARENT ONT::pain-reliever)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

