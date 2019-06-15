;;;;
;;;; W::bet
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bet
   (wordfeats (W::morph (:forms (-vb) :past W::bet)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (example "you can bet on that")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::BELIEVE)
     (example "I bet that I know what's going on")
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    ((meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     (LF-PARENT ONT::bet)
     (TEMPL AGENT-EXTENT-FORMAL-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::for W::on)))))
     (example "he bet 5 dollars on/for that horse")
     )
    ((meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     (LF-PARENT ONT::bet)
     (TEMPL AGENT-EXTENT-FORMAL-XP-TEMPL  (xp (% W::cp (W::ctype W::s-finite))))
     (example "he bet 5 dollars that his horse would win the race")
     )
    )
   )
))

