;;;;
;;;; W::secure
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::secure
   (SENSES
    ((meta-data :origin cmap-testing :entry-date 20090929 :change-date nil :comments nil :wn-sense (1) :vn ("defend-85"))
     (LF-PARENT ONT::protecting)
     (SEM (F::Time-span F::extended))
     (example "the bolt secured the door")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::secure
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050816 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::SAFE)
     (EXAMPLE "send a secure message")
     )
    )
   )
))

