;;;;
;;;; W::slunk
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ;; alternate past form
  (W::slunk
   (wordfeats (W::morph (:forms NIL)) (W::vform W::past))
   (SENSES
    ((LF-PARENT ONT::self-locomote)
     (TEMPL agent-TEMPL)
     )
    )
   )
))

