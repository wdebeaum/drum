;;;;
;;;; W::LEFT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::LEFT
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20060803 :change-date nil :comments nil :wn ("left%1:15:00"))
     (LF-PARENT ont::left-loc);ONT::object-dependent-location)
     (example "to the left of the building")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     ;; enforced subcat to reduce ambiguity, but prevents "on the left" unless we add another grammar rule
;;     (TEMPL other-reln-subcat-required-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LEFT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("left%5:00:00:unexhausted:00"))
     (LF-PARENT ONT::PART-WHOLE-VAL)
     (LF-FORM W::REMAINING)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::LEFT W::OVER)
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("left_over%5:00:00:unexhausted:00"))
     (LF-PARENT ONT::PART-WHOLE-VAL)
     (LF-FORM W::REMAINING)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("left_over%5:00:00:unexhausted:00"))
     (LF-PARENT ONT::PART-WHOLE-VAL)
     (LF-FORM W::REMAINING)
     (TEMPL postpositive-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
	  (w::left
	   (senses
	    ((lf-parent ONT::LEFT)
	     (templ attributive-only-adj-templ)
	     (meta-data :origin bee :entry-date 20040408 :change-date 20090731 :wn ("left%3:00:00") :comments test-s)
	     )
	    ))	  
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::LEFT
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

