;;;;
;;;; W::interview
;;;;
;;;;
;;;; W::INTERVIEW
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::interview
   (wordfeats (W::morph (:forms (-vb) :nom w::interview)))
   (SENSES
    ((LF-PARENT ONT::interview)
     (example "interview the candidate about it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-addressee-associated-information-templ)
     )
    ;; have to have this straight transitive sense for passive to work
    ((LF-PARENT ONT::interview)
     (example "interview the candidate")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-addressee-templ)
     )
    ((LF-PARENT ONT::interview)
     (example "he was interviewing all day")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-templ)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     )
    )
   )
))

