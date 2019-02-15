;;;;
;;;; w::intensify
;;;;


(define-words :pos W::V
  :templ agent-affected-xp-templ
 :words (
(w::intensify
 (senses
  ((meta-data :origin cause-result-relations :entry-date 20190108 :change-date nil :comments nil)
   (LF-PARENT ONT::intensify)
   (example "The latest change has intensified the problem.")
    )
    ((meta-data :origin cause-result-relations :entry-date 20190108 :change-date nil :comments nil)
     (EXAMPLE "The problem intensified")
     (LF-PARENT ONT::intensify)
     (TEMPL affected-unaccusative-templ)
     )
  )
 )
))