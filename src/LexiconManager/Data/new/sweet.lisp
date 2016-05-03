;;;;
;;;; w::sweet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::sweet W::potato)
   (wordfeats (W::morph (:forms (-S-3P) :plur (w::sweet w::potatoes))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050803 :change-date 20070809 :comments nil :wn ("sweet_potato%1:13:00"))
     (LF-PARENT ONT::vegetable)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::SWEET W::AND W::SOUR)
  (senses
	   ((LF-PARENT ONT::DRESSINGS-SAUCES-COATINGS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SWEET W::ONION)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SWEET W::PEPPER)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  ((W::sweet w::talk)
    (wordfeats (W::morph (:forms (-vb) :past (W::sweet w::talked) :ing (w::sweet w::talking))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
;     (LF-PARENT ont::provoke)
     (LF-PARENT ont::flatter)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He spurred him [to run for office]")  
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (w::sweet
   (wordfeats (W::MORPH (:FORMS (-LY -ER))))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ONT::taste-val)
     )
    )
   )
))

