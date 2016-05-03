;;;;
;;;; w::stress
;;;;

(define-words :pos W::n
 :words (
  (w::stress
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::stress
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  ((W::stress (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

