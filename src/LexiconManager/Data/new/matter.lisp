;;;;
;;;; W::matter
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::matter
   (SENSES
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :wn ("issue%1:09:01" "issue%1:09:00") :comments nil)
     (LF-PARENT ONT::situation)
     (example "see what is the matter; talk about the matter")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::matter
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL mass-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    (example "high cholesterol")
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::matter
   (SENSES
    ((EXAMPLE "Does it matter")
     (LF-PARENT ONT::SALIENCE)
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-TEMPL)
     )
    ((EXAMPLE "It doesn't matter that the switch is off")
     (LF-PARENT ONT::SALIENCE)
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL)
     (meta-data :origin beetle :entry-date 20050216 :change-date nil :comments mockup-1)
     )    
    ((EXAMPLE "It doesn't matter where the switch is located")
     (LF-PARENT ONT::SALIENCE)
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp2 (% w::NP (w::sort w::wh-desc))))
     (meta-data :origin beetle :entry-date 20050216 :change-date nil :comments mockup-1)
     )    
    ))
))

