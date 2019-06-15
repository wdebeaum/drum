;;;;
;;;; W::mislead
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::mislead
   (wordfeats (W::morph (:forms (-vb) :past W::misled :ing W::misleading)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like dare
     )
    )
   )
))

