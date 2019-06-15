;;;;
;;;; W::spend
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::spend
   (wordfeats (W::morph (:forms (-vb) :past W::spent :ing W::spending)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s11 :vn ("consume-65-1"))
     (LF-PARENT ONT::spend-TIME)
     (TEMPL NEUTRAL-EXTENT-TEMPL)
     (example "he spent 5 hours")
     )
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments nil :vn ("consume-65-1"))
       (LF-PARENT ONT::spend-time)
     (TEMPL AGENT-EXTENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::for W::on)))))
     (example "he spent 5 hours on/for it")
     )
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments nil)
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-EXTENT-TEMPL)
     (example "he spent 5 dollars")
     )
    ((meta-data :origin calo :entry-date 20031219 :change-date nil :comments nil)
     (LF-PARENT ONT::commerce-pay)
     (TEMPL AGENT-EXTENT-NEUTRAL-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::for W::on)))))
     (example "he spent 5 dollars on/for it")
     )
    )
   )
))

