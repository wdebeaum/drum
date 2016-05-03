;;;;
;;;; w::constant
;;;;

(define-words :pos w::N 
 :words (
;; Mathematical operations
  (w::constant
  (senses((LF-parent ONT::Mathematical-term) 
	    (templ bare-pred-templ)
	    (meta-data :origin lam :entry-date 20050420 :change-date nil :comments lam-initial)
	    ))
  ;; Added by Myrosia during LAM extension
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::constant
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :wn ("recurring%5:00:00:continual:00") :comments nil)
     (LF-PARENT ONT::continuous-VAL)
     )
    )
   )
))

