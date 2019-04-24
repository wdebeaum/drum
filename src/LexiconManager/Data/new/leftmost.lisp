;;;;
;;;; w::leftmost
;;;;

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (w::leftmost
     (senses
      ((lf-parent ONT::LEFT)
       (meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("leftmost%5:00:00:left:00") :comments nil)
       )
      ))	  
))
|#

#|
(define-words :pos W::adj 
 :tags (:base500)
 :words (
  (W::leftmost
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("leftmost%5:00:00:left:00") :comments nil)
     (LF-PARENT ONT::LEFT)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::in W::of)))))
     )
    ))))
|#

(define-words :pos W::adj 
 :tags (:base500)
 :words (
  (W::leftmost
   (wordfeats (W::COMPARATIVE W::SUPERL))
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090731 :wn ("leftmost%5:00:00:left:00") :comments nil)
     (LF-PARENT ONT::MAX-VAL)
     (LF-FORM W::LEFT)
     (TEMPL superl-templ)
     )
    ))))



