;;;;
;;;; W::CAUTIOUS
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::CAUTIOUS
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040920 :change-date nil :wn ("cautious%3:00:00") :comments caloy2)
     (lf-parent ont::careful-val)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin calo :entry-date 20040920 :change-date nil :wn ("careful%3:00:00") :comments caloy2)
     (lf-parent ont::careful-val)
     (example "a cautious letter")
     (templ central-adj-content-templ)
     (preference .98)
     )
    )
   )
))

