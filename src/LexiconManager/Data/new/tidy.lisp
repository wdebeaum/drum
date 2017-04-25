;;;;
;;;; W::tidy
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::tidy
   (SENSES
    ((LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "tidy the room")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((W::tidy (w::up))
  (SENSES
   ((LF-PARENT ONT::clean)
    (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
    (example "tidy up the room")
    (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
    )   
   ((meta-data :origin lam :entry-date 20050422 :change-date nil :comments lam-initial)
    (LF-PARENT ONT::revise)
    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
   )
  )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  (w::tidy
	   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
	   (senses
     ((lf-parent ont::tidy-val)
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :wn ("tidy%3:00:00") :comments lam-initial)
	     (example "a tidy solution")
	     )
	    ))
))

