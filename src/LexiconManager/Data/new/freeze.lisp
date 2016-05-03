;;;;
;;;; W::freeze
;;;;

(define-words :pos W::v 
 :words (
  (W::freeze
   (wordfeats (W::morph (:forms (-vb) :past W::froze :pastpart W::frozen :ing W::freezing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("freeze%2:43:10"))
     (LF-PARENT ont::atmospheric-event)
     (TEMPL expletive-templ) ; like rain
     )
    (
     (LF-PARENT ont::change-integrity)
     (TEMPL affected-templ)
     (example "the ice froze")
     )
    ((LF-PARENT ont::change-integrity)
     (TEMPL agent-affected-xp-templ)
     (example "I froze the ice")
     )
    )
   )
))

