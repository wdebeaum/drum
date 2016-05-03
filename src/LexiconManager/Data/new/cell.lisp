;;;;
;;;; w::cell
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::cell W::phone)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::cell W::phones))))
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2)
     (example "can I access it from my cell phone")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::cell
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("cell%1:06:04") :comments caloy2)
     (preference .98)
     (example "can I access it from my cell")
     )
    )
   )
))

(define-words :pos W::n
 :words (
;; internal
  (w::cell
   (senses((LF-PARENT ONT::cell)
	   (TEMPL count-pred-templ)
	   )
	  )
   )
  ))

(define-words :pos W::n
  :tags (bio)
  :words (
	  ((w::cell w::line)
	   (senses
	    ((LF-PARENT ONT::cell-line)
	     (TEMPL count-pred-templ)
	     )
	    )
	   )))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::cell
   (SENSES
    ((LF-PARENT ONT::shape-object)
     (EXAMPLE "merge the cells in a table")
     (meta-data :origin task-learning :entry-date 20050822 :change-date nil :comments nil)
     (preference .98)
     )
    )
   )
))

