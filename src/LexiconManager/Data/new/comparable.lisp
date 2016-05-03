;;;;
;;;; W::comparable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::comparable
    ;; can't auto-generate adv with theme templ
  ;; (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((EXAMPLE "png is comparable to gif")
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :wn ("comparable%5:00:00:same:00") :comments nil)
     (LF-PARENT ONT::SIMILARITY-VAL)
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE (? pt w::in w::to)))))
     )
    )
   )
))

