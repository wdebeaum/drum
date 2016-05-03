;;;;
;;;; w::grow
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (w::grow
   (wordfeats (W::morph (:forms (-vb) :ing W::growing :past W::grew :pastpart w::grown :nom growth)))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (lf-parent ont::become)
     (example "he grew restless")
     (templ affected-pred-templ)
     )
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil :vn ("grow-26.2") :wn ("grow%2:30:01" "grow%2:30:02" "grow%2:30:04" "grow%2:30:10" "grow%2:36:00"))
     (lf-parent ont::grow)
     (example "the grass grew")
     (templ affected-templ)
     )
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (lf-parent ont::grow)
     (example "he grew vegetables in his garden")
     (templ agent-affected-xp-templ)
     )
    )
   )
))

