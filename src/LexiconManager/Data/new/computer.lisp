;;;;
;;;; W::COMPUTER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::COMPUTER
   (SENSES
    ( (meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("computer%1:06:00") :comments calo-y1script)
     (LF-PARENT ONT::computer)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
;; Scientific (only math so far) disciplines
;; adding words for caloy3-test-data
  ((w::computer w::science)
  (senses((LF-parent ONT::Science-discipline) 
	    (templ mass-pred-templ)
	    (meta-data :origin lam :entry-date 20050420 :change-date nil :comments lam-initial)
	    ))
  ;; Added by Myrosia during LAM extension
)
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::computer W::online)
   (SENSES
    ((LF (W::online))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GO-ONLINE))
     (PREFERENCE .98)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::computer W::offline)
   (SENSES
    ((LF (W::offline))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GO-OFFLINE))
     (PREFERENCE .98)
     )
    )
   )
))

