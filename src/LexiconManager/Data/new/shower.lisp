;;;;
;;;; w::shower
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::shower
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("shower%1:06:00") :comments plow-req)
     (LF-PARENT ONT::fixture)
     )
    ((meta-data :origin asma :entry-date 20111005)
     (LF-PARENT ONT::shower)
     )
    ((LF-PARENT ONT::precipitation)
     (example "scattered showers")
     (meta-data :origin plow :entry-date 20060712 :change-date nil :wn ("shower%1:19:00") :comments plow-req)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::shower
   (wordfeats (W::morph (:forms (-vb) :past W::showered :ing W::showering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::gicing)
 ; like spray
     (example "shower them with roses")
     )
    )
   )
))

