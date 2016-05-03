;;;;
;;;; W::swallow
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::swallow
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("gobble-39.3-2"))
     (EXAMPLE "Swallow the pill")
     (LF-PARENT ONT::CONSUME)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("gobble-39.3-2"))
     (EXAMPLE "Don't swallow")
     (LF-PARENT ONT::CONSUME)
     (TEMPL AGENT-TEMPL)
     (PREFERENCE 0.98)
     )
    )
   )
))

