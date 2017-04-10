
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::prime
   (SENSES
    ((lf-parent ont::superior-val)
     (example "a prime location" "a prime cut of meat")
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((W::prime w::minister)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

