;;;;
;;;; W::ELSE
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::ELSE
   (SENSES
    ((LF-PARENT ONT::IDENTITY-VAL)
     (TEMPL ELSE-TEMPL)
     )
    )
   )
))

(define-words :pos W::cv :boost-word t
 :words (
;; is this still used?
;(define-list-of-words :pos W::cv :boost-word t
;  :senses (((LF W::CV)
;    (non-hierarchy-lf t)(TEMPL no-features-templ)
;    )
;   )
; :words (W::IF)
; )
  (W::ELSE
  (senses((LF W::IDENTITY)
    (non-hierarchy-lf t)(TEMPL no-features-templ)
    )
   )
)
))

