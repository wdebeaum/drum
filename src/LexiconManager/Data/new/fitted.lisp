;;;;
;;;; w::fitted
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ;; alternate past form
  (w::fitted
   (wordfeats (W::morph (:forms NIL)) (W::agr (? ag W::1s w::2s W::3s w::1p w::2p w::3p)))
   (senses
    ((EXAMPLE "He fitted it under the table")
     (LF-PARENT ONT::put)
     (syntax (w::vform (? vf w::past w::pastpart)))
     (templ agent-affected-xp-templ)
     )
    )
   )
))

