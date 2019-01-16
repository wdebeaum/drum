;;;;
;;;; W::hope
;;;;

(define-words :pos W::v 
 :words (
  (W::hope
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (example "He hoped to visit the islands")
     (TEMPL experiencer-theme-subjcontrol-templ)
     (PREFERENCE 0.98) ;; prefer noun sense?
     )
   
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (example "He hoped for a prize")
     (TEMPL experiencer-neutral-xp-templ (xp (% w::pp (w::ptype w::for)))) ; like wish
     (PREFERENCE 0.98)
     )
    )
   )
))

