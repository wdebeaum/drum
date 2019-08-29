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
     ;(LF-PARENT ont::change-integrity)
     (LF-PARENT ont::solidify)
     (TEMPL affected-templ)
     (example "the ice froze")
     )
    (;(LF-PARENT ont::change-integrity)
     (LF-PARENT ont::solidify)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "I froze the ice")
     )
    )
   )
))

(define-words :pos W::n
 :words (
    (W::freeze
     (SENSES
     ((meta-data :wn ("freeze%1:19:00"))
      (LF-PARENT ONT::weather)
      (templ COUNT-PRED-TEMPL)
      (example "kale is best harvested after a freeze")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
    (W::freezing
     (SENSES
      ((meta-data :wn ("freeze%2:43:10"))
       (LF-PARENT ONT::cold)
       (templ CENTRAL-ADJ-TEMPL)
       (example "it is freezing outside")
     )
    )
   )
))

