;;;;
;;;; W::bid
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::bid
   (SENSES
    ((LF-PARENT ONT::order)
     (meta-data :origin task-learning :entry-date 20050930 :change-date nil :wn ("bid%1:10:00") :comments nil)
     (example "your bid is no longer the highest bid")
     (templ other-reln-templ  (xp (% W::pp (W::ptype (? pt W::for w::on)))))
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::bid
   (wordfeats (W::morph (:forms (-vb) :past W::bid :ing W::bidding)))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050930 :change-date nil :comments nil)
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-EXTENT-TEMPL)
     (example "he bid 5 dollars")
     )
    ((meta-data :origin task-learning :entry-date 20050930 :change-date nil :comments nil)
     (LF-PARENT ONT::bet)
     (TEMPL AGENT-EXTENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::for W::on)))))
     (example "he bid 5 dollars on/for it")
     )
    ((meta-data :origin task-learning :entry-date 20050930 :change-date nil :comments nil)
     (LF-PARENT ONT::bet)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::for W::on)))))
     (example "he bid on/for it")
     )
    )
   )
))

