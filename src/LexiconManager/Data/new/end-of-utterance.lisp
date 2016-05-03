;;;;
;;;; W::end-of-utterance
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::end-of-utterance
   (SENSES
    ((LF (W::end-of-utterance))
     (non-hierarchy-lf t))
    )
   )
))

