;;;;
;;;; W::diagnose
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::diagnose
     (wordfeats (W::morph (:forms (-vb) :nom w::diagnosis)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("characterize-29.2") :wn ("diagnose%2:31:01"))
     (LF-PARENT ONT::classify)
     (TEMPL agent-neutral-as-theme-optional-templ) ; like interpret,classify
     )
    )
   )
))

