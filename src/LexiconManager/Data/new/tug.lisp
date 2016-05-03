;;;;
;;;; W::tug
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::tug
   (SENSES
;    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("carry-11.4") :wn ("tug%2:35:00" "tug%2:35:01" "tug%2:35:02" "tug%2:35:03" "tug%2:35:04"))
;     (LF-PARENT ONT::move)
; ; like drag
;     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("push-12-1"))
     (LF-PARENT ONT::pull)
     (TEMPL agent-affected-xp-templ) ; like pull
     )
    )
   )
))

