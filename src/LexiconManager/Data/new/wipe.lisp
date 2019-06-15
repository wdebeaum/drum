;;;;
;;;; W::wipe
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::wipe
   (SENSES
    (
     (LF-PARENT ONT::rub-scrape-wipe)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   ((W::wipe (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-tiredness)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

