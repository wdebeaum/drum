;;;;
;;;; W::march
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::march
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("march%2:38:00" "march%2:38:01" "march%2:38:04" "march%2:38:10"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

(define-words :pos W::name :boost-word t
 :words (
  (W::MARCH
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

