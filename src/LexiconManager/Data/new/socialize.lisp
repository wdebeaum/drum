;;;;
;;;; w::socialize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::socialize
   (SENSES
    ((LF-PARENT ONT::social-activity)
     (example "he socializes with them regularly")
     (TEMPL AGENT-with-co-agent-optional-TEMPL)
     )
    )
   )
))

