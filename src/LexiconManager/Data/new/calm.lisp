;;;;
;;;; W::calm
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((W::calm (w::down))
   (SENSES
    ((LF-PARENT ont::evoke-relief)
     (example "calm down")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     (preference .97) ;; prefer transitive
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::evoke-relief)
     (example "calm him down")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::calm
   (SENSES
    (;;(LF-PARENT ONT::managing)
     ; (lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (lf-parent  ont::taking-care-of)
     (example "calm the situation")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL)
     )
    ((LF-PARENT ont::evoke-relief)
     (example "music calms the savage beast")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin calo-ontology :entry-date 20050922 :change-date nil :comments vn2-integration)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::calm
   (SENSES
    ((meta-data :origin adjective-reorganization :entry-date 20170403 :change-date nil :comments nil :wn nil :comlex nil)
     (lf-parent ont::calm)
     )
    )
   )
))