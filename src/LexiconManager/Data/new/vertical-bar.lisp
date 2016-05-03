;;;;
;;;; W::vertical-bar
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
    ;; this is in the grammar?
   (W::vertical-bar
   (SENSES
    ((LF (W::vertical-bar))
     (non-hierarchy-lf t))
      )
   )
))

