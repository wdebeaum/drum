;;;;
;;;; W::punc-comma
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-comma
   (SENSES
    ((LF (W::COMMA))
     ;; beetle-fix: allow comma skipping to simplify parsing 09/08/06
     ;;     (syntax (w::skip +))
     (non-hierarchy-lf t))
    )
   )
))

