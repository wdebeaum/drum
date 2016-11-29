;;;;
;;;; W::near
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::near
   (SENSES
    ((LF-PARENT ONT::MOVE-toward)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the truck/I neared the corner")
     (TEMPL agent-neutral-xp-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::NEAR
   (wordfeats (W::MORPH (:FORMS (-ER))) (W::COMP-OP W::LESS))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("near%3:00:00"))
     (example "not the near one, the far one")
     (LF-PARENT ONT::near)
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation ont::less) (f::intensity ont::hi))
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::NEAR
   (SENSES
    ((LF-PARENT ONT::time-clock-rel)
     (LF-FORM W::APPROXIMATE)
     (example "it is near five pm")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ont::near-reln) ; ONT::proximity
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "find a hotel near a zipcode" "he is near the party")
     ;(SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    ((LF-PARENT ont::near-reln) ; ONT::proximity
     (TEMPL BINARY-CONSTRAINT-S-trajectory-TEMPL)
     (example "move it near the triangle")
     (preference .98)
     ;(SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    )
   )
))

