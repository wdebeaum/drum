;;;;
;;;; W::clean
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::clean
   (SENSES
   
    ((LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "clean the room")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     )
    )
   )
))

#||(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((W::clean (w::up))
   (SENSES
    ((LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "clean up the room")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     )
    )
   )
))||#

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::clean (W::out))
   (SENSES
    ;;;; clean out the truck
    ((LF-PARENT ONT::clean)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::clean
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :wn ("clean%3:00:01") :comments caloy3)
     (LF-PARENT ONT::cleanliness-val)
     (example "are the rooms clean")
     )
    )
   )
))

