;;;;
;;;; w::grow
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (w::grow
   (wordfeats (W::morph (:forms (-vb) :ing W::growing :past W::grew :pastpart w::grown
				:nom w::growth :nomsubjpreps (w::of) :nomobjpreps -)))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (lf-parent ont::become)
     (example "he grew restless")
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL)
     )
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil :vn ("grow-26.2") :wn ("grow%2:30:01" "grow%2:30:02" "grow%2:30:04" "grow%2:30:10" "grow%2:36:00"))
     (lf-parent ont::grow)
     (example "the grass grew")
     (templ affected-templ)
     )
    )
   )
))


(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (w::grow
   (wordfeats (W::morph (:forms (-vb) :ing W::growing :past W::grew :pastpart w::grown
				:nom w::growth )))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (lf-parent ont::grow)
     (example "he grew vegetables in his garden")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

