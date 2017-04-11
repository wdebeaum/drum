;;;;
;;;; W::DARK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::DARK
   (SENSES
    ((LF-PARENT ONT::light)
     (example "they were sitting in the dark")
     (meta-data :origin calo :entry-date 20050811 :change-date nil :wn ("dark%1:15:00") :comments nil)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::DARK
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("dark%3:00:01"))
     (LF-PARENT ONT::DARK-VAL)
     (TEMPL LESS-ADJ-TEMPL)
     (example "they were sitting in a dark corner")
     )
    ((LF-PARENT ONT::dark-in-color-val)
     (example "dark green")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )    
    )
   )
))
