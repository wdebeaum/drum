;;;;
;;;; W::prepare
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::prepare
   (SENSES
    (
     (LF-PARENT ONT::PREPARE) ;; GUM change new parent 20121027
     (templ agent-affected-xp-templ) ;; GUM change new template 20121027
     (example "prepare software to be linked with application programs")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil :vn ("preparing-26.3-1"))
     )
    )
   )
))

