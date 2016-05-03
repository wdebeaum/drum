;;;;
;;;; W::punc-question-mark
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-question-mark
   (SENSES
    ((LF (W::QUESTION-MARK))
     (non-hierarchy-lf t)(SYNTAX (W::punctype (? x W::ynq W::whq)))
     )
    )
   )
))

