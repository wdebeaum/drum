;;;;
;;;; W::alter
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::alter
   (wordfeats (W::morph (:forms (-vb) :nom w::alteration)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("turn-26.6.1") :wn ("alter%2:30:01"))
;     (LF-PARENT ONT::modify)
     (LF-PARENT ONT::change)

     )
    ((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (LF-PARENT ONT::change)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "it changed in color / with time" "the room changed")
     (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
      )
   ))
))

