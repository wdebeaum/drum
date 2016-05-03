;;;;
;;;; w::gather
;;;;

(define-words :pos W::v 
 :words (
  (w::gather
   (wordfeats (W::morph (:forms (-vb) :past W::gathered :ing w::gathering)))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("herd-47.5.2") :wn ("gather%2:30:00" "gather%2:35:00" "gather%2:38:00" "gather%2:41:00" "gather%2:41:02"))
     (LF-PARENT ONT::meet)
     (example "they gathered at the church")
     (TEMPL agent-plural-templ)
     )
    ((LF-PARENT ONT::collect)
     (example "gather the wood")
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("shake-22.3-2"))
     (TEMPL AGENT-affected-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((w::gather (w::up))
   (wordfeats (W::morph (:forms (-vb) :past W::gathered :ing w::gathering)))
   (senses
    ((LF-PARENT ONT::collect)
     (example "gather up the pieces")
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     (TEMPL AGENT-affected-XP-TEMPL)
     )
    )
   )
))

