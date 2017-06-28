;;;;
;;;; W::enter
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::enter
   (wordfeats (W::morph (:forms (-vb) :past W::entered :ing W::entering)))
   (SENSES
    ((EXAMPLE "the vehicle entered the enclosure")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Penetrate)
     (LF-PARENT ONT::entering)
     (templ agent-neutral-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((EXAMPLE "enter the title in the textbox")
     (meta-data :origin calo :entry-date 20050621 :change-date nil :comments plow)
     (LF-PARENT ONT::put)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
     ((EXAMPLE "enter!")
     (meta-data :origin gloss-training :entry-date 20100301 :change-date nil :comments nil)
     (LF-PARENT ONT::entering)
     (templ agent-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )

    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "We enter into an agreement.")
     ;(templ agent-goal-optional-templ (xp (% W::PP (W::ptype (? t W::into)))))
     (templ agent-templ)
     )

     ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "We enter the contest")
     (templ agent-neutral-templ)
     )
   ))
))

