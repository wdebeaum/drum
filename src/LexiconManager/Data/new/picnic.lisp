;;;;
;;;; w::picnic
;;;;

(define-words :pos W::n
 :words (
;; type of located event -- you can 'go to' them
  (w::picnic
  (senses
   ((LF-PARENT ONT::gathering-event)
    (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::picnic
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ONT::consume)
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL  (xp (% W::pp (W::ptype W::on)))) ; like dine
     )
    )
   )
))

