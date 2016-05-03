;;;;
;;;; W::obligate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::obligate
   (wordfeats (W::morph (:forms (-vb) :nom w::obligation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He obligated him [to run for office]")  
     )
    )
   )
))

