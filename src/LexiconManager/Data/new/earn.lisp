;;;;
;;;; W::earn
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::earn
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1-1"))
     (LF-PARENT ONT::acquire)
     (templ agent-affected-xp-templ) ; like get but no to-recipient
     (preference .98) ;; try specific sense first
     )
    ;; this sense requires a theme that is ont::object-function ont::currency
    ((meta-data :origin step :entry-date 20081022 :change-date nil :comments nil)
     (LF-PARENT ONT::earning)
     (example "he earned money")
     )
    )
   )
))

