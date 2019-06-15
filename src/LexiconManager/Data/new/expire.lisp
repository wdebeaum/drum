;;;;
;;;; W::expire
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::expire
   (wordfeats (W::morph (:forms (-vb) :nom w::expiration)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disappearance-48.2") :wn ("expire%2:30:00" "expire%2:42:00"))
     (LF-PARENT ONT::die)
     (TEMPL affected-templ) ; like die
     )
    ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (example "the permit expired")
     (TEMPL affected-templ) ; like stop
     )
    )
   )
))

