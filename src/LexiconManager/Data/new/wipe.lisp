;;;;
;;;; W::wipe
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
   (W::wipe
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::clean)
     (TEMPL agent-affected-xp-templ) 
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   ((W::wipe (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-tiredness)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

