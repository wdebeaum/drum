;;;;
;;;; W::wind
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::wind
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("wind%1:19:00"))
     (LF-PARENT ONT::air-current)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::wind
   (wordfeats (W::morph (:forms (-vb) :past W::wound :ing W::winding)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("wind%2:35:06" "wind%2:38:00"))
     (LF-PARENT ONT::rotate)
     (example "it wound around the fence")
     (TEMPL affected-templ) ; like move,bounce
     )
    )
   )
))

