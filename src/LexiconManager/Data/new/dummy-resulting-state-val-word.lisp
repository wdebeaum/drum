;;;;
;;;; W::dummy-resulting-state-val-word
;;;;

; not sure this ONT::resulting-state-val should exist but put something here so it can have some default templates 
(define-words :pos W::adj
 :words (
  (W::dummy-resulting-state-val-word
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    (
     (LF-PARENT ONT::resulting-state-val)
     (TEMPL central-adj-templ)
     (example "an xyz book")
     )
    )))
 )
