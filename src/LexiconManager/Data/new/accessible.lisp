;;;;
;;;; W::accessible
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::accessible
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("accessible%3:00:00"))
     (lf-parent ont::accessible-val)
     (example "the routes are accessible by helicopter")
     (SEM (F::GRADABILITY +))
     (TEMPL central-adj-xp-templ (XP (% W::PP (W::PTYPE (? pt W::on W::for w::by)))))
     )
    )
   )
))

