;;;;
;;;; W::analogous
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::analogous
    ;; can't auto-generate adv with theme templ
   ;;(wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((EXAMPLE "linking a program to a library is analogous to running a program")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090731 :wn ("analogous%5:00:00:similar:00") :comments nil)
     (LF-PARENT ONT::SIMILAR)
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE (? pt w::in w::to)))))
     )
    )
   )
))

