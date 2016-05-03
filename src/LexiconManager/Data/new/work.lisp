;;;;
;;;; W::WORK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::WORK
   (SENSES
    ; nom
;    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("work%1:04:00"))
;     (LF-PARENT ONT::working)
;     (example "he went to work" "his work phone")
;     (templ mass-pred-templ)
;     )
    ((meta-data :origin calo :entry-date 20040423 :change-date nil :wn ("work%1:06:00") :comments caloy1v6)
     (LF-PARENT ONT::information-function-object)
     (example "a big disk to store my work")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 ((W::work W::out)
  (wordfeats (W::morph (:forms (-vb) :nom w::workout)))
   (SENSES
    ((LF-PARENT ONT::working-out)
     (example "he works out every day")
     (TEMPL agent-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::work (W::out))
   (SENSES
    ((LF-PARENT ONT::determine)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     (example "he worked out that he already knew the answer")
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::solve)
     (example "work out a plan" "work out when to go")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::work (W::through))
   (SENSES
    ;;;; swier -- work through the plan
    ((LF-PARENT ONT::information-scrutiny)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::WORK
   (wordfeats (W::morph (:forms (-vb) :nom w::work)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (example "he worked on the problem")
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-theme-xp-templ (xp (% W::PP (W::ptype (? p W::on)))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (example "he worked all day")
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-templ)
     )    
    ;;;; swift 04/09/02 need a sense that doesn't require a human subject, e.g. 'it/the plan works'
    ((LF-PARENT ONT::FUNCTION)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-TEMPL)
     )
    (
     (lf-parent ont::work) ;;  20120524 GUM change new parent 
     (example "he works for that company")
     (meta-data :origin LbR :entry-date 20081124 :change-date 20090508 :comments nil :wn nil)
     (templ agent-neutral-xp-templ (xp (% W::PP (W::ptype (? p W::for w::at)))))
     )
    )
   )
))

