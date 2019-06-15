;;;;
;;;; w::sneeze
;;;;

(define-words :pos W::n
 :words (
  (w::sneeze
  (senses
   ((meta-data :wn ("sneeze%1:26:00"))
    (LF-PARENT ONT::sneeze)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::sneeze
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ont::exhale-forcefully)
     (TEMPL affected-templ)
     (EXAMPLE "He covered his mouth and sneezed.")
     )
    )
   )
))

