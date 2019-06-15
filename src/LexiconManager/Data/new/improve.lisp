;;;;
;;;; W::improve
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::improve
   (SENSES
    ((example "this feature improves the performance")
     (sem (f::aspect f::dynamic))
     (meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
     (LF-PARENT ONT::improve)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((example "the performance improved")
     (sem (f::aspect f::dynamic))
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     (templ affected-templ)
     )
    )
   )
))

