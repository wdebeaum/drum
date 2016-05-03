;;;;
;;;; W::WHEEL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::WHEEL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("wheel%1:06:00"))
;     (LF-PARENT ONT::WHEEL)
     (LF-PARENT ONT::VEHICLE-PART)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
	  ;; need a new name for this -- ont::wheel is taken
	  (W::wheel
	   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("drive-11.5") :wn ("wheel%2:38:04"))
;     (LF-PARENT ONT::wheel-drive) ; like drive
     (LF-PARENT ONT::transport) ; like drive
     )
        )
	   )
))

