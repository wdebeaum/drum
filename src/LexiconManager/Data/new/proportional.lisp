;;;;
;;;; W::proportional
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::proportional
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::measure-related-property-val)
     (meta-data :origin task-learning :entry-date 20051028 :change-date 20090915 :wn ("proportional%5:00:00:proportionate:00") :comments nil)
     (EXAMPLE "the space is proportional to font size")
     (SEM (F::GRADABILITY -))
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::to))))
     )
    )
   )
))

