;;;;
;;;; W::fetch
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::fetch
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1-1"))
     (LF-PARENT ONT::acquire)
     (templ agent-affected-xp-templ) ; like get but no to-recipient
     )
    )
   )
))

