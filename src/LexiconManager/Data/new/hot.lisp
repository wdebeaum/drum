;;;;
;;;; W::hot
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::hot w::spot)
   (SENSES
    ((meta-data :origin plow :entry-date 20071024 :change-date nil :comments nil)
     (LF-PARENT ONT::hotspot)
     (example "a wireless hotspot")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::HOT W::CHILI W::PEPPER)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::HOT
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("hot%3:00:01") :comments html-purchasing-corpus)
      (LF-PARENT ONT::WARM)
     )
    )
   )
))

