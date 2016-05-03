;;;;
;;;; W::Confident
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::Confident
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::CERTAIN)
     (example "he is confident that he will win")
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-finite))))
     (meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("confident%3:00:00") :comments caloy2)
     )
    )
   )
))

