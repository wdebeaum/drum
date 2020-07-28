;;;;
;;;; W::dream
;;;;

(define-words :pos W::v
 :words (
  (W::dream
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-formal-xp-templ (xp (% w::cp (w::ctype w::s-that)))) 
     )
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-formal-xp-templ (xp (% w::pp (w::ptype w::of)))) ; like wish but w::of instead of w::for
     )
    )
   )
))

