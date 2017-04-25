;;;;
;;;; w::king
;;;; 

(define-words :pos W::n :templ count-pred-templ
 :tags (:base500)
 :words (
(W::king
   (SENSES
    ((meta-data :origin bolt :entry-date 20120516 :comments top500)
     (LF-PARENT ONT::person)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::king
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (example "a room with a king bed")
     (lf-parent ont::predefined-size-val)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (w::king
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

