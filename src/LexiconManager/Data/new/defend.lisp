;;;;
;;;; w::defend
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
       (w::defend
     (senses
      ((lf-parent ont::fighting)
       (example "he defended the proposal")
       (templ agent-neutral-xp-templ)
       (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
       )
      ))
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::defend
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("defend-85"))
     (LF-PARENT ONT::protecting)
 ; like protect
     )
    )
   )
))

