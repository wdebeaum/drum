;;;;
;;;; W::check
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::check
   (SENSES
    ((LF-PARENT ONT::graphic-symbol)
     (example "click on the check box")
     (meta-data :origin plow :entry-date 20070918 :change-date nil :comments nil)
     (preference .97)
     )
     ((LF-PARENT ONT::scrutiny)
     (example "a lab check")
     (meta-data :origin cernl :entry-date 20100701 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::check w::up)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090407 :change-date 20050817 :wn ("appointment%1:14:00") :comments nil)
     (LF-PARENT ONT::gathering-event)
     ("confirm the appointment" "go to the appointment")
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::check
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("search-35.2") :wn ("check%2:31:02" "check%2:32:10"))
     (LF-PARENT ONT::scrutiny)
     (example "check the plan")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    ((LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ (xp (% W::pp (W::ptype W::on))))
     )
    ((meta-data :origin calo :entry-date 20040413 :change-date nil :comments y1v3)
     (LF-PARENT ONT::scrutiny)
     (example "let me check")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-templ)
     (PREFERENCE 0.96) ;; disprefer intransitive
     )
    ((EXAMPLE "Check or uncheck the selected calendar in the Calendars list")
     (LF-PARENT ONT::select)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil)
     (LF-PARENT ONT::SELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "check red")
     (TEMPL AGENT-FORMAL-TEMPL)
     )
    ((LF-PARENT ONT::determine)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (example "check whether/if/that it's a book order")
     (meta-data :origin plow :entry-date 20050909 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
 ((W::check w::in)
    (wordfeats (W::morph (:forms (-vb) :nom (W::check w::in))))
   (SENSES
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "let me show you how to check in a patient")
     (meta-data :origin plot :entry-date 20081207 :change-date nil :comments nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "when can I check in (at the hotel)")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (TEMPL AGENT-RESULT-XP-NP-TEMPL (xp (% W::PP (W::ptype (? t W::to W::at)))))
     (preference .98)
     )
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "when can I check in" "he checked in yesterday")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (templ agent-templ)
     (preference .97)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
 ((W::check w::out)
  (wordfeats (W::morph (:forms (-vb) :nom (W::check w::out))))
   (SENSES
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "let me show you how to check out a patient")
     (meta-data :origin plot :entry-date 20081207 :change-date nil :comments nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
   
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "when can I check out" "he checked out yesterday")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (templ agent-templ)
     (preference .97)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::check (W::out))
   (SENSES
    ((EXAMPLE "I want amigo to check out this area")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin lou :entry-date 20031106)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::check (W::on))
   (SENSES
    ((EXAMPLE "check on the situation")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin calo :entry-date 20040915 :change-date nil :comments caloy2)
     )
    )
   )
))

