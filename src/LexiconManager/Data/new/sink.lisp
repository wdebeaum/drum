;;;;
;;;; w::sink
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::sink
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("sink%1:06:00") :comments plow-req)
   ;  (LF-PARENT ONT::fixture)
     (lf-parent ont::sink)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::sink
   (wordfeats (W::morph (:forms (-vb) :past W::sank :pastpart W::sunk :ing W::sinking :nom w::sink)))
   (SENSES
    ((LF-PARENT ONT::move-downward)
     (EXAMPLE "He sank the boat")
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("other_cos-45.4") :wn ("sink%2:38:02" "sink%2:38:00"))
     )
    ((LF-PARENT ONT::move-downward)
     (TEMPL AFFECTED-TEMPL)
     (EXAMPLE "The boat sank")
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("other_cos-45.4") :wn ("sink%2:38:02" "sink%2:38:00"))
     )
    )
   )
))

