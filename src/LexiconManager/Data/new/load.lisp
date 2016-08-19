;;;;
;;;; W::LOAD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LOAD
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::CONTAINER-LOAD)
     (TEMPL Classifier-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	 (w::load
	  (senses ((lf-parent ont::device-component)
		   (Example "A load is a lightbulb -- the electrical sense only")
		   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s)
		   )))
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::LOAD
   ;; here the goal is subcategorized for because the meanings of in/into seem not to vary
   ;; and we preserve isomorphism with the representation of the 'with' alternation
   ;; however we treat other spatial prepositions as mods (adverbials) and preserve the meanings
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("spray-9.7-2"))
;     (LF-PARENT ONT::move)
     (LF-PARENT ONT::FILL-CONTAINER)
     (example "load the oj into the tanker")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-goal-optional-templ)
     )

    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::Fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "load the truck with oj")
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

#|
(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::load (W::up))
   (SENSES
    ;;;; swier -- load up the oj
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ;;;; swier -- load up the truck with oj
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))
|#

