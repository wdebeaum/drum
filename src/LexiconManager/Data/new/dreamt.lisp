;;;;
;;;; W::dreamt
;;;;

(define-words :pos W::v 
 :words (
  (W::dreamt
   (wordfeats (W::morph (:forms NIL)) (W::vform (? vf W::past)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like wish
     )
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-theme-xp-templ (xp (% w::pp (w::ptype w::of)))) ; like wish but w::of instead of w::for
     )
    )
   )
))

