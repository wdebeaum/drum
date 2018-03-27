;;;;
;;;; W::DOWNSTREAM
;;;;

(define-words :pos W::ADV
 :words (
   (W::DOWNSTREAM
   (SENSES
    ((LF-PARENT ONT::DIRECTION-down-ground)
     (TEMPL PRED-S-POST-TEMPL)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     )
    )
   )
))


(define-words :pos W::adj
 :words (
   (W::DOWNSTREAM
    (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
    (SENSES
    ((LF-PARENT ONT::DIRECTION-down-ground )
     (TEMPL adj-CO-THEME-templ (XP (% W::PP (W::PTYPE (? p W::of w::from)))))
     (example "The box downstream of that.")
     )
    )
   )
))

