;;;;
;;;; W::drop
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::drop
   (wordfeats (W::morph (:forms (-vb) :nom W::drop)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_direction-9.4") :wn ("drop%2:38:01"))
     (LF-PARENT ONT::move-downward)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like raise,lower
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("drop%2:38:00" "drop%2:38:01"))
     (LF-PARENT ONT::move-downward)
     (TEMPL affected-templ) ; like move,bounce
     (PREFERENCE 0.96)
     )
    ;; delete the following -- we get the MOVE-DOWNWARD reading even with it!
    #|((LF-PARENT ONT::decrease)
     (example "it dropped in temperature")
     (TEMPL AFFECTED-FORMAL-XP-OPTIONAL-TEMPL  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
     )|#
    
    ((meta-data :origin calo-ontology :entry-date 20060710 :change-date nil :comments nil)
     (LF-PARENT ONT::discard)
     (example "drop this paragraph from the text")
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from))))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::drop (W::off))
   (wordfeats (W::morph (:forms (-vb) :ing W::dropping)))
    (SENSES
    ((LF-PARENT ONT::UNLOAD)
     (example "drop off the oranges")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
    )
))

