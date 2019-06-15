;;;;
;;;; w::greet
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::greet
    (wordfeats (W::morph (:forms (-vb) :nom w::greeting)))
   (senses
    ((LF-PARENT ONT::greet)
     (TEMPL AGENT-AGENT1-NP-TEMPL)
     (EXAMPLE "he greeted them")
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090508 :comments caloy3 :vn ("judgement-33") :wn ("?greet%2:31:00" "greet%2:32:00"))
     )    
    )
   )
))

