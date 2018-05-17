;;;;
;;;; W::farthest
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::farthest
   (wordfeats (W::COMPARATIVE W::SUPERL)); (W::FUNCTN ONT::linear-scale))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("farther%5:00:01:far:00"))
     (LF-PARENT ONT::max-val)
     (lf-form w::far)
     (TEMPL SUPERL-TWOSUBCATS-TEMPL (xp (% W::pp (W::ptype w::from))))
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi) (F::SCALE ONT::DISTANCE-SCALE) ;ONT::linear-scale)
	  )
     )
    )
   )
))

