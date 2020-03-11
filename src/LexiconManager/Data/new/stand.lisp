;;;;
;;;; w::stand
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (w::stand
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil)
     (LF-PARENT ont::support-stand)
     (example "plant stand")
     )
    ((meta-data :origin caet :entry-date 20111220)
     (lf-parent ont::base) ;stand)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::stand
   (wordfeats (W::morph (:forms (-vb) :past W::stood :ing W::standing)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (EXAMPLE "The tree was standing on the hill")
     (LF-PARENT ONT::BE-AT-LOC) 
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::Extended))
     (TEMPL neutral-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::stand (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::stood :ing W::standing)))
   (SENSES
    ((EXAMPLE "You had better stand up")
     (LF-PARENT ONT::BODY-MOVEMENT-place)
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::contest-deny-oppose-protest)
     (example "he stood up to cancer")
     (TEMPL AGENT-neutral-xp-TEMPL  (xp (% W::pp (W::ptype W::to))))
     )


    )
   )
))

