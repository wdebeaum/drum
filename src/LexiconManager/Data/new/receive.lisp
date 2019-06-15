;;;;
;;;; W::receive
;;;;

(define-words :pos W::v 
 :words (
  (W::receive
   (SENSES
    (;;(lf-parent ont::acquire)
     (lf-parent ont::incur-inherit-receive) ;; 20120524 GUM change new parent
     (example "he received the package")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL)
     )
    )
   )
))

