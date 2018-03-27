;;;;
;;;; w::together
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (w::together
   (SENSES
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL pred-s-vp-TEMPL)
     (example "They dined together.")
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    ;; both together, how much is that together

    (
     (lf-parent ont::adjacent)
     (TEMPL PRED-NP-TEMPL)
     (example "put them together")
     )

    )
   )
))

