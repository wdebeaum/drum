;;;;
;;;; W::SHOW
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SHOW
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::presentation)
     (preference .97)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::SHOW
   (wordfeats (W::morph (:forms (-vb) :pastpart W::shown)))
   (SENSES
     ((LF-PARENT ONT::show)
     (example "show him how to buy a book")
     ;(TEMPL agent-affected-iobj-theme-templ)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL (xp (% NP)))
     )
    ((LF-PARENT ONT::show)
     (example "show him (the house)")
     ;(TEMPL AGENT-AFFECTED-IOBJ-NEUTRAL-TEMPL)
     (TEMPL AGENT-AGENT1-NEUTRAL-2-XP1-3-XP-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::show)
     (example "show the house (to him)")
     ;(TEMPL AGENT-NEUTRAL-TOAFFECTED-TEMPL)
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::confirm)
     (example "this diagram shows that it works")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite))))
    )
    ((LF-PARENT ONT::confirm)
     (example "We show in this paper that it works")
     (preference .98)
     (TEMPL AGENT-FORMAL-LOCATION-2-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite))))
     )
    
    ((LF-PARENT ONT::confirm)
     (example "I showed it to be broken")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
    )
    ((LF-PARENT ONT::correlation)
     (example "these results show that the gene activates the protein")
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
    )

    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::show W::up)
   (wordfeats (W::morph (:forms (-vb) :pastpart W::shown)))
   (SENSES 
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("appear-48.1.1"))
     (LF-PARENT ONT::appear)
     (TEMPL affected-result-xp-TEMPL )
     (preference .98)
     )
       )
   )
))

