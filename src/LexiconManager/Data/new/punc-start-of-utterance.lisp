;;;;
;;;; W::punc-start-of-utterance
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-start-of-utterance
   (SENSES
    ((LF (W::start-of-utterance))
     (non-hierarchy-lf t))
    )
   )
))

