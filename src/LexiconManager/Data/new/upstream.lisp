;;;;
;;;; W::UPSTREAM
;;;;

(define-words :pos W::ADV
 :words (
   (W::UPSTREAM
   (SENSES
    ((LF-PARENT ONT::DIRECTION-up-ground)
     (TEMPL PRED-S-POST-TEMPL)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
   (W::UPSTREAM
    (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
    (SENSES
    ((LF-PARENT ONT::DIRECTION-up-ground )
     (TEMPL adj-CO-THEME-templ (XP (% W::PP (W::PTYPE (? p W::of w::from)))))
     (example "The box upstream of that.")
     )
    )
   )
))
