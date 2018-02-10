;;;;
;;;; w::rightmost
;;;;

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (w::rightmost
     (senses
      ((lf-parent ONT::RIGHT)
       (meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("rightmost%5:00:00:right:00") :comments nil)
       )
      ))
))
|#

(define-words :pos W::adj 
 :tags (:base500)
 :words (
  (W::rightmost
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("rightmost%5:00:00:right:00") :comments nil)
     (LF-PARENT ONT::RIGHT)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::in W::of)))))
     )
    ))))

