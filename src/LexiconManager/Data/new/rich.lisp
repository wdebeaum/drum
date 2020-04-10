;;;;
;;;; W::RICH
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::RICH
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("rich%3:00:00") :comments html-purchasing-corpus)
     (lf-parent ont::wealthy-val)
     )
    )
   )
))

(define-words :pos W::adj 
  :words (
  (W::RICH
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((EXAMPLE "It is rich in vitamin B12")
     (LF-PARENT ONT::abundant-val)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::Ptype W::in))))
     )
     ))
  )
  )
