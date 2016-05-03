;;;;
;;;; W::hasten
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::hasten
   (wordfeats (W::morph (:forms (-vb) :past W::hastened :ing W::hastening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("?hasten%2:30:00" "hasten%2:38:00"))
     (LF-PARENT ONT::move-rapidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    ((lf-parent ont::increase-speed)
     (example "hasten the process")
     (templ agent-affected-xp-templ)
    )
    )
   )
))

