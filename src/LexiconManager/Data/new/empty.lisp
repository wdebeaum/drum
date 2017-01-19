;;;;
;;;; W::EMPTY
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::EMPTY
   (wordfeats (W::morph (:forms (-vb) :past W::emptied)))
   (SENSES
    ((LF-PARENT ONT::EMPTY)
     (example "empty the oj from the tanker")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-source-templ)
     )

    ((LF-PARENT ONT::EMPTY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "empty the truck of oj")
     (TEMPL AGENT-source-affected-optional-TEMPL)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090529 :comments nil :vn ("clear-10.3-1"))
     (LF-PARENT ONT::empty)
     (TEMPL affected-templ ) ; like drain
     )
    )
   )
  ))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::empty (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::emptied)))
   (SENSES  
    ((EXAMPLE "empty out the truck")
     (LF-PARENT ONT::UNLOAD)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
          )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::EMPTY
    (wordfeats (W::morph (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090915 :comments nil :wn ("empty%3:00:00"))
     (LF-PARENT ONT::unfilled)
     )
    )
   )
))

