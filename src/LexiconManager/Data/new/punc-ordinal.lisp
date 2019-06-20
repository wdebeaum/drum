;;;;
;;;; W::punc-ordinal
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::punc-ordinal
   (SENSES
    ((LF (W::ORDINAL))
     (non-hierarchy-lf t))
    )
   )
  ))

; 1st
(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::st
   (SENSES
    ((LF (W::ORDINAL))
     (non-hierarchy-lf t))
    )
   )
))

; 2nd
(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::nd
   (SENSES
    ((LF (W::ORDINAL))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::rd
   (SENSES
    ((LF (W::ORDINAL))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::th
   (SENSES
    ((LF (W::ORDINAL))
     (non-hierarchy-lf t))
    )
   )
))


