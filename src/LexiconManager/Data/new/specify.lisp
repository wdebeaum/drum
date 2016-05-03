;;;;
;;;; W::specify
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::specify
   (wordfeats (W::morph (:forms (-vb) :nom w::specification)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("characterize-29.2"))
     (LF-PARENT ONT::select)
     (TEMPL agent-theme-xp-templ) ; like select
     (PREFERENCE 0.96)
     )
    ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil)
     (LF-PARENT ONT::SELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "specify red")
     (templ agent-theme-pred-templ)
     )
    ((LF-PARENT ONT::describe)
     (example "he specified the requirements")
     (meta-data :origin task-learning :entry-date 20050812 :change-date 20090506 :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ agent-neutral-xp-templ)
     )
    )
   )
))

