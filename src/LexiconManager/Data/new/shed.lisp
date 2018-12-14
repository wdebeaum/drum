;;;;
;;;; W::shed
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::shed
   (SENSES
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil)
     (LF-PARENT ONT::lodging)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::shed
   (wordfeats (W::morph (:forms (-vb) :past W::shed :ing W::shedding)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("substance_emission-43.4") :wn ("shed%2:35:02" "shed%2:35:00" "shed%2:29:00"))
     (EXAMPLE "The dog is shedding")
     (LF-PARENT ONT::shed)
     (TEMPL agent-TEMPL)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("substance_emission-43.4") :wn ("shed%2:35:02" "shed%2:35:00" "shed%2:29:00"))
     (EXAMPLE "The snake shed its skin")
     (LF-PARENT ONT::shed)
     (TEMPL agent-affected-xp-TEMPL)
     )
    )
   )
))

