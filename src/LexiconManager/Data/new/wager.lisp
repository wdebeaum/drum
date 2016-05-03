;;;;
;;;; W::wager
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::wager
   (wordfeats (W::morph (:forms (-vb) :past W::wagered :ing W::wagering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

