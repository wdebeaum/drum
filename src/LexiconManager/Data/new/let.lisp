;;;;
;;;; w::let
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((w::let (w::off))
   (wordfeats (W::morph (:forms (-vb) :past W::let)))
   (senses
    ((LF-PARENT ont::releasing)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug lets off a spark")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((w::let (w::out))
   (wordfeats (W::morph (:forms (-vb) :past W::let)))
   (senses
    ((LF-PARENT ont::releasing)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug lets out a spark")
     (TEMPL agent-affected-xp-templ)
          )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((w::let w::go)
   (wordfeats (W::morph (:forms (-vb) :past W::let)))
   (senses
    ((EXAMPLE "let go of the problem")
     (LF-PARENT ont::releasing)
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Release-Resources)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-XP-TEMPL (xp (% W::pp (W::ptype W::of))))
     
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::let
   (wordfeats (W::morph (:forms (-vb) :past W::let :ing W::letting)))
   (SENSES
    ((LF-PARENT ONT::ALLOW)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "let them eat cake" "let me teach you how to dance")
     (TEMPL agent-effect-affected-OBJCONTROL-TEMPL (xp (% W::VP (W::vform W::base))))
     )
    ;; this should have an implicit effect
    ((LF-PARENT ONT::ALLOW)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "let them")
     (preference .95)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::let W::me W::see)
   (SENSES
    ((LF (W::LET-SEE))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EVALUATION))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ;; changed - we know expand the contraction ^s to US
  ((W::let W::us W::see)
   (SENSES
    ((LF (W::LET-SEE))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EVALUATION))
     )
    )
   )
))

