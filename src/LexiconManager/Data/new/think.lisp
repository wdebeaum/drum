;;;;
;;;; W::think
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::think
   (wordfeats (W::morph (:forms (-vb) :past W::thought)))
   (SENSES
    ;;;; swier -- I think that this plan is better.
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2"))
     (LF-PARENT ONT::believe)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ;;;; swier -- I didn't think of that.
    ((LF-PARENT ONT::cogitation)
     (meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :wn ("think%2:31:01:"))
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% W::PP (W::ptype (? p W::of W::about)))))
     )
    ;;;; I think
    ((LF-PARENT ONT::believe)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-templ)
     (preference .96)
     )
    ((LF-PARENT ONT::believe)
     (example " they thought her to have cancer")
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-objcontrol-templ)
     )
    ;; need this to get the dobj gap
    ((LF-PARENT ONT::believe)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "what do you think")
     (TEMPL experiencer-theme-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::think (W::through))
   (wordfeats (W::morph (:forms (-vb) :past W::thought)))
   (SENSES
    ;;;; THINK THROUGH THE PLAN!!!
    ((LF-PARENT ONT::information-scrutiny)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

