;;;;
;;;; w::crack
;;;;

(define-words :pos W::n
 :words (
  (w::crack
  (senses
	   ((LF-PARENT ONT::event)
	    (TEMPL COUNT-PRED-TEMPL)
	    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments nil)
	    )
	   )
  ;; crack of the whip, crack of dawn
)
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::crack
   (wordfeats (W::morph (:forms (-vb) :nom W::crack)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("crack%2:30:00" "crack%2:30:01" "crack%2:30:02" "crack%2:30:05"))
     (LF-PARENT ont::break-object)
 ; like tear
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("crack%2:30:00" "crack%2:30:01" "crack%2:30:02" "crack%2:30:05"))
     (LF-PARENT ont::break-object)
     (TEMPL agent-affected-xp-templ) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("crack%2:30:00" "crack%2:30:01" "crack%2:30:02" "crack%2:30:05"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
       )
)))

