;;;;
;;;; W::guard
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::guard
   (SENSES
    ((meta-data :origin cmap-testing :entry-date 20090929 :change-date nil :comments nil)
     (LF-PARENT ONT::protecting)
     (SEM (F::Time-span F::extended))
     (example "the fence guarded the compound")
     (templ agent-affected-xp-templ)
     (preference .98) ;; prefer agentive
     )
    )
   )
))

