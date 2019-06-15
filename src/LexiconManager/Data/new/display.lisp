;;;;
;;;; W::DISPLAY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DISPLAY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("display%1:06:01"))
     (LF-PARENT ONT::DISPLAY)
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::DISPLAY
   (SENSES
    ((LF-PARENT ONT::visual-display)
     )
    ((LF-PARENT ONT::encodes-message)
     (meta-data :origin "bee" :entry-date 20091394 :change-date nil :comments nil)
     (example "this diagram displays a short circuit")
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL)
     (Preference 0.97) ;; choose agentive interpretation whenever possible
     )
    )
   )
))

