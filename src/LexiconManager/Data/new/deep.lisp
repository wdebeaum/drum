;;;;
;;;; W::deep
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::deep w::fry)
      (wordfeats (W::morph (:forms (-vb) :past (W::deep w::fried) :ing( W::deep w::frying) )))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("deep-fry%2:30:00"))
     (LF-PARENT ONT::cooking)
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
;   )
   (W::DEEP
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("deep%3:00:01") :comments html-purchasing-corpus)
     (LF-PARENT ONT::linear-dimension)
     (TEMPL LESS-ADJ-TEMPL)
     (example "a deep hole")
     )
    ((meta-data :origin cardiac :entry-date 20090130 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::intense)
     (TEMPL LESS-ADJ-TEMPL)
     (example "deep breathing")
     )
    )
   )
))

