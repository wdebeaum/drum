;;;;
;;;; w::critical
;;;;

(define-words :pos W::adj :templ adj-purpose-optional-templ
 :words (
   (W::CRITICAL
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("critical%5:00:00:indispensable:00") :comments html-purchasing-corpus)
     (lf-parent ont::urgent-val)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype w::for w::to))))
     )
    )
   )
))

