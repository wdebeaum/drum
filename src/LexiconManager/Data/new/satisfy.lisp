;;;;
;;;; w::satisfy
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::satisfy
    (senses
     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("satisfy%2:37:00"))
       (LF-PARENT ONT::subduing)
       (TEMPL agent-affected-xp-templ)
       (PREFERENCE 0.96)
       )
     ((LF-PARENT ONT::Compliance)
      (example "satisfy the requirements")
      (templ agent-theme-xp-templ)
      (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil)
      )
     ))
))

