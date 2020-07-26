;;;;
;;;; W::reckon
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::reckon
   (wordfeats (W::morph (:forms (-vb) :past W::reckoned :ing W::reckoning)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::believe)
     (TEMPL experiencer-formal-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

