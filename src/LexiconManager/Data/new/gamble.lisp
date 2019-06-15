;;;;
;;;; W::gamble
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::gamble
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

