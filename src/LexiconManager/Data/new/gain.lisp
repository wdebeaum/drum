;;;;
;;;; W::GAIN
;;;;

#||(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::GAIN
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("gain%1:23:00"))
     (LF-PARENT ONT::acquire)
     (example "he experienced weight gain")
     (TEMPL other-reln-theme-TEMPL)
     )
    )
   )
))||#

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::gain
   (wordfeats (W::morph (:forms (-vb) :nom w::gain)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1-1"))
     (LF-PARENT ONT::acquire)
     (templ agent-affected-xp-templ) ; like get but no to-recipient
     )
    )
   )
))

