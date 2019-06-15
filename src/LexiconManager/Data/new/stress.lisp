;;;;
;;;; w::stress
;;;;

(define-words :pos W::n
 :words (
  (w::stress
  (senses
   ((meta-data :wn ("stress%1:26:01"))
    (LF-PARENT ONT::stress-scale)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::stress
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-distress)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::stress (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-distress)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

