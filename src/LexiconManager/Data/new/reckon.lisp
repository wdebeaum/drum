;;;;
;;;; W::reckon
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::reckon
   (wordfeats (W::morph (:forms (-vb) :past W::reckoned :ing W::reckoning)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::believe)
     (TEMPL experiencer-theme-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

