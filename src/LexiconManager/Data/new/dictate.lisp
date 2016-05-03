;;;;
;;;; W::dictate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::dictate
   (wordfeats (W::morph (:forms (-vb) :nom w::dictation)))
   (SENSES
    ((LF-PARENT ONT::extended-say)
     (example "He dictated a letter to her")
     (TEMPL agent-affected-iobj-theme-templ)
     (meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date nil :comments nil :vn ("transfer_mesg-37.1-1") :wn ("dictate%2:31:00"))
     )
    )
   )
))

