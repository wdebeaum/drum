
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::bold
   (SENSES
    ((LF-PARENT ONT::font)
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("bold%1:10:00") :comments nil)
     (example "make the text bold")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::bold
   (SENSES
    ((meta-data :origin calo :entry-date 20040915 :change-date nil :wn ("bold%3:00:00") :comments caloy2)
     (lf-parent ont::bold-val)
     (example "bold person")
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date nil :wn ("ambitious%3:00:00") :comments caloy2)
     (lf-parent ont::bold-val)
     (example "bold idea")
     (templ central-adj-content-templ)
     )
    )
   )
))

