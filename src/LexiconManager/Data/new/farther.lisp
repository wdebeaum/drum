;;;;
;;;; W::farther
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::farther
   (wordfeats (W::COMPARATIVE +)); (W::FUNCTN ONT::linear-scale))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("farther%5:00:01:far:00"))
     (LF-PARENT ONT::MORE-VAL)
     (lf-form w::far)
     ;(TEMPL COMPAR-TEMPL)
     (TEMPL COMPAR-twosubcats-TEMPL (xp (% W::pp (W::ptype w::from))))
     (SEM (f::orientation F::pos) (f::intensity ont::med) (F::SCALE ONT::DISTANCE-SCALE) ;ONT::linear-scale)
	  )
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::FARTHER
   (SENSES
    ((LF-PARENT ONT::EXTENSION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20060111 :change-date nil :comments nil)
     (example "move it a little bit farther down")
     )
    )
   )
))

