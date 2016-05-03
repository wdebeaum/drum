;;;;
;;;; w::sneeze
;;;;

(define-words :pos W::n
 :words (
  (w::sneeze
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::sneeze
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2") :wn ("sneeze%2:29:00"))
     (LF-PARENT ont::nonverbal-expression)
     (TEMPL agent-templ) ; like laugh
     )
    )
   )
))

