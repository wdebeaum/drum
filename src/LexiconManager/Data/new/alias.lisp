;;;;
;;;; W::alias
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::alias
   (SENSES
    ((LF-PARENT ONT::name)
     (templ other-reln-theme-templ (xp (% W::pp (W::ptype (? pt W::for w::of)))))
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("alias%1:10:00") :comments nil)
     (example "The first parameter is an alias specifying the file to display")
     )
    )
   )
))

