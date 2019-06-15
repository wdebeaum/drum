;;;;
;;;; W::dictate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dictate
   (wordfeats (W::morph (:forms (-vb) :nom w::dictation)))
   (SENSES
    ((LF-PARENT ONT::extended-say)
     (example "He dictated a letter to her")
     (TEMPL AGENT-FORMAL-AFFECTED-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date nil :comments nil :vn ("transfer_mesg-37.1-1") :wn ("dictate%2:31:00"))
     )
    )
   )
))

