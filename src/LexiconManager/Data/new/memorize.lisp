;;;;
;;;; W::memorize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::memorize
   (wordfeats (W::morph (:forms (-vb) :past W::memorized :ing w::memorizing :nom w::memorization)))
   (SENSES
    ((LF-PARENT ONT::memorize)
     (meta-data :origin quicken :entry-date 20071129 :change-date nil :comments nil :wn ("memorize%2:31:00"))
     (example "he memorized everything")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

