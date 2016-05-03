;;;;
;;;; W::zonk
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::zonk
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-tiredness)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   ((W::zonk (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-tiredness)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

