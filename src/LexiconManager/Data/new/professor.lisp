;;;;
;;;; W::professor
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::professor
   (SENSES
    ((meta-data :origin calo :entry-date 20050404 :change-date nil :wn ("professor%1:18:00") :comments projector-purchasing)
     (LF-PARENT ONT::teacher-trainer) ;professional)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (w::professor
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

