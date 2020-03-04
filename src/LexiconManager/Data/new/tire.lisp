;;;;
;;;; W::tire
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::tire
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("tire%2:37:01"))
     (LF-PARENT ont::evoke-tiredness) ;cause-body-effect)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::tire (w::out))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090511 :comments LM-vocab)
     (LF-PARENT ONT::evoke-tiredness) ;cause-body-effect)
     (syntax (w::resultative +)) 
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

