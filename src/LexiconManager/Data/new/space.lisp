;;;;
;;;; W::space
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::space
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20050427 :change-date nil :wn ("space%1:25:00") :comments fruitcart-11-4)
     (example "put more space between the bananas")
     (LF-PARENT ONT::substance)
     (TEMPL MASS-PRED-TEMPL)
     )
     ((example "we entered the space")
      (LF-PARENT ONT::loc-as-area)
      )
     ((example "type space")
      (preference .98)
      (LF-PARENT ONT::letter-symbol)
     )
    )
   )
))



(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
	      :words (
		      (W::space
		       (SENSES
			((EXAMPLE "Space your doses three hours apart ")
			 (LF-PARENT ONT::SPACE)
			 
			 )
			)
		       )
		      ))

