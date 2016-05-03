;;;;
;;;; w::booking
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ;; we need to define the nominalization to be able to use determiners
   (w::booking
   (SENSES
    ((meta-data :origin plot :entry-date 20081112 :change-date nil :comments nil :wn ("booking%1:04:00"))
     (LF-PARENT ont::reserve)
     (templ other-reln-affected-templ  (xp (% W::pp (W::ptype (? pt W::for w::of)))))
     (example "let me show you how to do a booking for a patient")
     )
    )
   )
))

