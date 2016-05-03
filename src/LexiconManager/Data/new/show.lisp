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

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::SHOW
   (wordfeats (W::morph (:forms (-vb) :pastpart W::shown)))
   (SENSES
   ; handled by ont::show
 ;   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("indicate-76-1-1"))
 ;    (LF-PARENT ONT::confirm)
 ;    (example "show that it works")
 ;    (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like confirm
 ;    (PREFERENCE 0.97)
 ;    )
; changed this to ont::show to allow physical-object themes in addition to all those allowed by this sense.
;    ((LF-PARENT ONT::transfer-information)
;     (example "show me how to find a book")
;     (TEMPL agent-affected-iobj-theme-templ)
;     )

    ((LF-PARENT ONT::show)
     (example "show the house")
     )
    ((LF-PARENT ONT::show)
     (example "show him the house / how to buy a book")
     (TEMPL agent-affected-iobj-theme-templ)
     )
    ((LF-PARENT ONT::confirm)
     (example "this diagram shows that it works")
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
    )
    ((LF-PARENT ONT::confirm)
     (example "We show in this paper that it works")
     (preference .98)
     (TEMPL agent-located-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     )
    
    ((LF-PARENT ONT::confirm)
     (example "I showed it to be broken")
     (TEMPL agent-effect-affected-objcontrol-templ)
    )
    ((LF-PARENT ONT::correlation)
     (example "these results show that the gene activates the protein")
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
    )

    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::show W::up)
   (wordfeats (W::morph (:forms (-vb) :pastpart W::shown)))
   (SENSES 
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("appear-48.1.1"))
     (LF-PARENT ONT::ARRIVE)
     (TEMPL affected-result-xp-TEMPL )
     (preference .98)
     )
       )
   )
))

