;;;;
;;;; W::motivate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::motivate
   (wordfeats (W::morph (:forms (-vb) :nom w::motivation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He motivated him [to run for office]")  
     )
    )
   )
))

