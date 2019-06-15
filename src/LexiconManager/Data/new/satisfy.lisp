;;;;
;;;; w::satisfy
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::satisfy
    (senses
     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("satisfy%2:37:00"))
       (LF-PARENT ONT::evoke-satisfaction)
       (EXAMPLE "I was satisfied to hear that he agreed with me")
       (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
       (PREFERENCE 0.96)
       )
     ((LF-PARENT ONT::Compliance)
      (example "satisfy the requirements")
      (TEMPL AGENT-FORMAL-XP-TEMPL)
      (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil)
      )
     ))
))

