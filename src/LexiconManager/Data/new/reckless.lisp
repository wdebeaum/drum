
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::reckless
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040920 :change-date nil :wn ("reckless%5:00:00:careless:00") :comments caloy2)
     (lf-parent ont::not-careful-val)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin calo :entry-date 20040920 :change-date nil :wn ("careful%3:00:00") :comments caloy2)
     (lf-parent ont::not-careful-val)
     (example "a reckless note")
     (templ central-adj-content-templ)
     (preference .98)
     )
    )
   )
))

