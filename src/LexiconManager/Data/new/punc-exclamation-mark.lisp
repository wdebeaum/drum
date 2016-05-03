;;;;
;;;; W::punc-exclamation-mark
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-exclamation-mark
   (SENSES
    ((LF (W::EXCLAMATION-MARK))
     (non-hierarchy-lf t)(SYNTAX (W::punctype (? x W::decl W::imp)))
     )
    )
   )
))

