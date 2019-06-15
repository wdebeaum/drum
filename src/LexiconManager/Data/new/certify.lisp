;;;;
;;;; W::certify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::certify
   (SENSES
    ((example "he was certified insane")
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("characterize-29.2") :wn ("certify%2:32:01"))
     (LF-PARENT ONT::categorization)
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-OPTIONAL-TEMPL) ; like interpret,classify
     )

    ((example "I am certified to practice law in this state")
     (LF-PARENT ONT::categorization)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )

    )
   )
))

