;;;;
;;;; W::register
;;;;

(define-words :pos W::v :templ AGENT-NEUTRAL-XP-TEMPL
 :words (
   (W::register
     (wordfeats (W::morph (:forms (-vb) :past W::registered :ing w::registering)))
   (SENSES
    
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :vn ("register-54.1") :wn ("register%2:32:00"))
     (EXAMPLE "register the data")
     (LF-PARENT ONT::RECORD)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "register for the conference/at the hotel")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL (xp (% W::PP (W::ptype (? t W::at W::for)))))
     )
    )
   )
))

